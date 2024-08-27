"Overall Flow:
The user calls gpt-copilot. This sets up the windows and prepares everything

They send a request. We send the full prompt, but the gpt chat buffer only shows what they requested
The response they get out excludes the patch
"

;; TODO #1 - figure out contexts with the anthropic API
"TODO make sure that the entire buffer isn't getting sent each time:
  - The gptel-request function calls gptel--create-prompt each time
  - I'll need a way to make sure we only send the minimal amount of
  - data and save context instead
  - Look at how gptel-add is implemented - it adds context to the gptel-context--alist,
    which is then sent over

  - If the API itself can't save context, make sure that I'm setting up the context--alist
    with my initial prompts - otherwise only the buffer gets sent, which doesn't include that info
"
;; TODO ability to clear the cache and start over
;; TODO easily save and reload the AI memory
;; - aka ability to have a context for different projects

;; TODO Function to toggle the gptel copilot
  ;; - this will need to check which project you're in - use projectile or project

;; TODO copy contexts of those cursor prompts into an initial context setup
;; TODO make sure everything is well documented and extensible

(require 'cl-generic)

(defvar gptel-lookup--history nil)

(defvar my-coding-assistant-chat-buffer
  "Buffer for the coding assistant chat.")

(defcustom gpt-copilot-window-size 0.33
  "Size of the GPT Copilot chat window as a fraction of the frame.
Must be a number between 0 and 1, exclusive."
  :type 'float
  :group 'gpt-copilot
  :set (lambda (symbol value)
         (if (and (numberp value)
                  (< 0 value 1))
             (set-default symbol value)
           (user-error "gpt-copilot-window-size must be a number between 0 and 1, exclusive")))
  :initialize 'custom-initialize-default)


(defvar gpt-copilot-window-orientation 'vertical
  "Orientation of the GPT Copilot chat window. Can be 'vertical or 'horizontal.")

;; TODO delete
(define-minor-mode gpt-copilot-mode
  "Minor mode for GPT Copilot integration."
  :lighter " GPT-Copilot"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-g") #'gptel-copilot-query)
            map))

(defvar my-coding-assistant-chat-buffer
  "Buffer for the coding assistant chat.")

(defvar gpt-base-prompt
  (concat
   ;; The prompt is originally from avante.nvim:
   ;; https://github.com/yetone/avante.nvim/blob/main/lua/avante/llm.lua
   "Your primary task is to suggest code modifications by creating a git patch. Follow these instructions meticulously:\n\n"
   "1. Carefully analyze the original code, paying close attention to its structure and line numbers. Line numbers start from 1 and include ALL lines, even empty ones.\n\n"
   "2. When suggesting modifications:\n"
   "   a. Use the language in the question to reply. If there are non-English parts in the question, use the language of those parts.\n"
   "   b. Explain why the change is necessary or beneficial.\n"
   "   c. Make absolutely certain that the code you provide is in the format of a git patch\n"
   "   d. Provide the exact code snippet to be replaced using this format:\n\n"
   "Git patch:\n"
   "```\n"
   "{{suggested_code}}\n"
   "```\n\n"

   "{{In-depth explanation of each change}}\n"
   "The git patch should be the first thing in the response, followed by the explanation"
   "4. Crucial guidelines for suggested code snippets:\n"
   "   - Only apply the change(s) suggested by the most recent message (before your generation).\n"
   "   - Do not make any unrelated changes to the code.\n"
   "   - Do not arbitrarily delete pre-existing comments/empty Lines.\n"
   "   - Do not omit any needed changes from the requisite messages/code blocks.\n"
   "   - If there is a clicked code block, bias towards just applying that (and applying other changes implied).\n"
   "   - Please keep your suggested code changes minimal, and do not include irrelevant lines in the code snippet.\n\n"
   "5. Crucial guidelines for creating the git patch:\n"
   "   - For multi-line changes, ensure the range covers ALL affected lines, from the very first to the very last.\n"
   "   - Double-check that your line numbers align perfectly with the original code structure.\n\n"
   "6. Final check:\n"
   "   - Review all suggestions, ensuring each line number is correct, especially the start_line and end_line.\n"
   "   - Confirm that no unrelated code is accidentally modified or deleted.\n"
   "   - Verify that the start_line and end_line correctly include all intended lines for replacement.\n"
   "   - Perform a final alignment check to ensure your line numbers haven't shifted, especially the start_line.\n"
   "   - Double-check that your line numbers align perfectly with the original code structure.\n"
   "   - Do not show the full content after these modifications.\n\n"
   "Remember: Accurate line numbers are CRITICAL. The range start_line to end_line must include ALL lines to be replaced,\n"
   "from the very first to the very last. Double-check every range before finalizing your response, paying special attention\n"
   "to the start_line to ensure it hasn't shifted down. Ensure that your line numbers perfectly match the original code structure without any overall shift."))

;; TODO function to toggle
(defun gpt-copilot-setup-windows ()
  "Set up the coding assistant layout with the chat window."
  (interactive)
  (delete-other-windows)
  (let* ((main-buffer (current-buffer))
         (main-window (selected-window))
         (split-size (floor (* (if (eq gpt-copilot-window-orientation 'vertical)
                                   (frame-width)
                                 (frame-height))
                               (- 1 gpt-copilot-window-size)))))
    (setq gpt-copilot-chat-buffer
          (gptel "*gptel-Copilot*"))
    (with-current-buffer gpt-copilot-chat-buffer
      (gpt-copilot-mode 1))
    (if (eq gpt-copilot-window-orientation 'vertical)
        (split-window-right split-size)
      (split-window-below split-size))
    (set-window-buffer main-window main-buffer)
    (other-window 1)
    (set-window-buffer (selected-window) gpt-copilot-chat-buffer)))

(defun gptel-copilot-query (user-query)
  "Send a query to the GPTel Copilot from the current buffer."
  (interactive "sUser Query: ")
  (unless (buffer-live-p gpt-copilot-chat-buffer)
    (gpt-copilot-setup-windows))
  ;; TODO need to make code-buffer more robust
  (let* ((code-buffer (if (eq (current-buffer) gpt-copilot-chat-buffer)
                          (window-buffer (next-window))
                        (current-buffer)))
         (selected-code (with-current-buffer code-buffer
                          (if (use-region-p)
                              (buffer-substring-no-properties (region-beginning) (region-end))
                            (buffer-substring-no-properties (point-min) (point-max)))))
         (file-type (with-current-buffer code-buffer
                      (symbol-name major-mode)))
         (full-query (format "%s\n\nFile type: %s\n\nCode:\n%s\n\n%s"
                             gpt-base-prompt
                             file-type
                             selected-code
                             user-query)))
    (with-current-buffer gpt-copilot-chat-buffer
      (goto-char (point-max))
      (insert user-query "\n")
      (gptel-request
          full-query
        :callback #'gptel-copilot-handle-response
        :position (point-marker)))))

(defun gptel-copilot-handle-response (response info)
  "Handle the response from the GPTel Copilot."
  (when response
    (let* ((code-buffer (plist-get info :buffer))
           (chat-buffer gpt-copilot-chat-buffer)
           (split-index (string-match "```\n\\'" response))
           (patch (when split-index
                    (substring response 0 (match-beginning 0))))
           (explanation (if split-index
                            (substring response (match-end 0))
                          response)))
      ;; Apply patch to code buffer
      (when patch
        (with-current-buffer code-buffer
          (gpt-copilot-apply-patch patch code-buffer)))

      ;; Insert explanation into chat buffer
      (with-current-buffer chat-buffer
        (let ((explanation-info (list :buffer chat-buffer
                                      :position (point-max-marker)
                                      :in-place t)))
          (gptel--insert-response (string-trim explanation) explanation-info))

        ;; Add a message in the chat buffer indicating that a patch was applied
        (when patch
          (let ((patch-message-info (list :buffer chat-buffer
                                          :position (point-max-marker)
                                          :in-place t)))
            (gptel--insert-response "A patch has been applied to the code buffer." patch-message-info)))))))

(defun gpt-copilot-extract-patch (response)
  "Extract the git patch from the GPT response."
  (when (string-match "```diff\n\\(\\(?:.\\|\n\\)*?\\)```" response)
    (match-string 1 response)))

(defun gpt-copilot-apply-patch (patch code-buffer)
  "Apply the git patch to the current buffer using git apply."
  (message patch)
  (let ((process-environment (cons "GIT_DIFF_OPTS=--unified=3" process-environment))
        (default-directory (or (vc-git-root default-directory)
                               default-directory))
        (coding-system-for-write 'utf-8)
        (coding-system-for-read 'utf-8)
        (process-connection-type nil))
    (with-temp-buffer
      (insert patch)
      (let ((exit-code
             (call-process-region (point-min) (point-max)
                                  "git" nil t nil
                                  "apply" "--ignore-whitespace" "--unidiff-zero" "-")))
        (if (zerop exit-code)
            (progn
              (message "Patch applied successfully")
              (with-current-buffer (current-buffer)
                (revert-buffer t t t)))
          (message "Failed to apply patch: %s"
                   (buffer-substring-no-properties (point-min) (point-max))))))))

(provide 'gptel-copilot)
