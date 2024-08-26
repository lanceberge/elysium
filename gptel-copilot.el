"Overall Flow:
The user calls gpt-copilot. This sets up the windows and prepares everything

They send a request. We send the full prompt, but the gpt chat buffer only shows what they requested
The response they get out excludes the patch
"

;; TODO ability to clear the cache and start over
;; TODO ability to chat with the bot in addition to getting patches
;; TODO easily save and reload the AI memory
;; TODO Function to toggle the gptel copilot
;; TODO ability to just send a region, not the whole buffer
(require 'cl-generic)

(defvar gptel-lookup--history nil)

(defvar my-coding-assistant-chat-buffer
  "Buffer for the coding assistant chat.")

(defvar gpt-base-prompt
  (concat
   "Your primary task is to suggest code modifications by creating a git patch. Follow these instructions meticulously:\n\n"
   "1. Carefully analyze the original code, paying close attention to its structure and line numbers. Line numbers start from 1 and include ALL lines, even empty ones.\n\n"
   "2. When suggesting modifications:\n"
   "   a. Use the language in the question to reply. If there are non-English parts in the question, use the language of those parts.\n"
   "   b. Explain why the change is necessary or beneficial.\n"
   "   c. Make absolutely certain that the code you provide is in the format of a git patch\n"
   "   d. Provide the exact code snippet to be replaced using this format:\n\n"
   "Git patch:\n"
   "```{{language}}\n"
   "{{suggested_code}}\n"
   "```\n\n"
   "3. Crucial guidelines for suggested code snippets:\n"
   "   - Only apply the change(s) suggested by the most recent assistant message (before your generation).\n"
   "   - Do not make any unrelated changes to the code.\n"
   "   - Produce a valid full rewrite of the entire original file without skipping any lines. Do not be lazy!\n"
   "   - Do not arbitrarily delete pre-existing comments/empty Lines.\n"
   "   - Do not omit large parts of the original file for no reason.\n"
   "   - Do not omit any needed changes from the requisite messages/code blocks.\n"
   "   - If there is a clicked code block, bias towards just applying that (and applying other changes implied).\n"
   "   - Please keep your suggested code changes minimal, and do not include irrelevant lines in the code snippet.\n\n"
   "4. Crucial guidelines for line numbers:\n"
   "   - The content regarding line numbers MUST strictly follow the format \"Replace lines: {{start_line}}-{{end_line}}\". Do not be lazy!\n"
   "   - The range {{start_line}}-{{end_line}} is INCLUSIVE. Both start_line and end_line are included in the replacement.\n"
   "   - Count EVERY line, including empty lines and comments lines, comments. Do not be lazy!\n"
   "   - For single-line changes, use the same number for start and end lines.\n"
   "   - For multi-line changes, ensure the range covers ALL affected lines, from the very first to the very last.\n"
   "   - Double-check that your line numbers align perfectly with the original code structure.\n\n"
   "5. Final check:\n"
   "   - Review all suggestions, ensuring each line number is correct, especially the start_line and end_line.\n"
   "   - Confirm that no unrelated code is accidentally modified or deleted.\n"
   "   - Verify that the start_line and end_line correctly include all intended lines for replacement.\n"
   "   - Perform a final alignment check to ensure your line numbers haven't shifted, especially the start_line.\n"
   "   - Double-check that your line numbers align perfectly with the original code structure.\n"
   "   - Do not show the full content after these modifications.\n\n"
   "Remember: Accurate line numbers are CRITICAL. The range start_line to end_line must include ALL lines to be replaced,\n"
   "from the very first to the very last. Double-check every range before finalizing your response, paying special attention\n"
   "to the start_line to ensure it hasn't shifted down. Ensure that your line numbers perfectly match the original code structure without any overall shift."))

(defun gpt-copilot (prompt)
  (interactive (list (read-string "Ask GPT: " nil gptel-lookup--history)))
  (when (string= prompt "") (user-error "A prompt is required."))
  prompt)

(defun gpt-copilot-wrapper ()
  (call-interactively #'gpt-copilot))

(defun gpt-copilot-setup ()
  "Set up the coding assistant layout with the chat window occupying 1/3 of the screen."
  (interactive)
  (delete-other-windows)
  (let* ((main-buffer (current-buffer))
         (frame-width (frame-width))
         (main-window-width (floor (* frame-width 0.67))))  ; 2/3 of the screen width
    (setq gpt-copilot-chat-buffer
          (gptel "*gptel-Copilot*"))

    ;; TODO apply the minor mode
    ;; Split the window and set its width to 2/3 of the frame
    (let ((main-window (selected-window)))
      (split-window-right main-window-width)
      (set-window-buffer main-window main-buffer))
    ;; Select the chat window (right side) and set its buffer
    (other-window 1)
    (set-window-buffer (selected-window) gpt-copilot-chat-buffer)))

(defun gptel-copilot-query (user-query)
  "Send a query to the GPTel Copilot from the current code buffer."
  (interactive "sEnter your query: ")
  (unless (buffer-live-p gpt-copilot-chat-buffer)
    (error "GPTel Copilot chat is not set up. Run gptel-copilot-setup first"))
  (let* ((code-buffer (current-buffer))
         (code-content (if (use-region-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))
                         (buffer-substring-no-properties (point-min) (point-max))))
         (file-type (symbol-name major-mode))
         (full-query (format "%s\n\nFile type: %s\n\nCode:\n%s\n\n%s"
                             gpt-base-prompt
                             file-type
                             code-content
                             user-query)))
    (with-current-buffer gpt-copilot-chat-buffer
      (goto-char (point-max))
      (insert user-query "\n\nAssistant: ")
      (gptel-request
          full-query
        :callback #'gptel-copilot-handle-response
        :buffer code-buffer
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
          (gpt-copilot-apply-patch patch)))

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

(defun gptel-copilot-apply-changes ()
  "Apply changes suggested by GPTel Copilot to the original buffer, overriding the existing code."
  (interactive)
  (let* ((chat-buffer gptel-copilot-chat-buffer)
         (code-buffer (get-buffer-window-list chat-buffer nil t)))
    (unless (and chat-buffer (buffer-live-p chat-buffer))
      (error "GPTel Copilot chat buffer not found"))
    (unless code-buffer
      (error "No visible buffer associated with GPTel Copilot chat"))
    (setq code-buffer (window-buffer (car code-buffer)))
    (with-current-buffer chat-buffer
      (goto-char (point-min))
      (while (re-search-forward "Replace lines: \\([0-9]+\\)-\\([0-9]+\\)\n```[^\n]*\n\\(\\(?:.\\|\n\\)*?\\)```" nil t)
        (let* ((start-line (string-to-number (match-string 1)))
               (end-line (string-to-number (match-string 2)))
               (new-code (match-string 3)))
          (with-current-buffer code-buffer
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- start-line))
              (let ((beg (point)))
                (forward-line (- end-line start-line -1))
                (delete-region beg (point))
                (insert new-code)))))))
    (message "Changes applied directly to the code buffer.")))

(defun gpt-copilot-apply-patch (patch)
  "Apply the git patch to the current buffer using git apply."
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
