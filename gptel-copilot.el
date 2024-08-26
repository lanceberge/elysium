(require 'cl-generic)

;; TODO M-x gptel-copilot pops up gptel in the right, sends buffer context
;; TODO send requests either in the box or inline in the buffer
;; TODO rewrites will be supplied only in the chat box, writes will be inline

;; TODO custom minor mode
;; TODO hardest part, patching the changes in

;; TODO send the buffer only once

(defvar gptel-lookup--history nil)

(defvar my-coding-assistant-chat-buffer
  "Buffer for the coding assistant chat.")

(defvar gpt-base-prompt
  "Your primary task is to suggest code modifications with precise line number ranges. Follow these instructions meticulously:

1. Carefully analyze the original code, paying close attention to its structure and line numbers. Line numbers start from 1 and include ALL lines, even empty ones.

2. When suggesting modifications:
   a. Use the language in the question to reply. If there are non-English parts in the question, use the language of those parts.
   b. Explain why the change is necessary or beneficial.
   c. Provide the exact code snippet to be replaced using this format:

Replace lines: {{start_line}}-{{end_line}}
```{{language}}
{{suggested_code}}
```

3. Crucial guidelines for suggested code snippets:
   - Only apply the change(s) suggested by the most recent assistant message (before your generation).
   - Do not make any unrelated changes to the code.
   - Produce a valid full rewrite of the entire original file without skipping any lines. Do not be lazy!
   - Do not arbitrarily delete pre-existing comments/empty Lines.
   - Do not omit large parts of the original file for no reason.
   - Do not omit any needed changes from the requisite messages/code blocks.
   - If there is a clicked code block, bias towards just applying that (and applying other changes implied).
   - Please keep your suggested code changes minimal, and do not include irrelevant lines in the code snippet.

4. Crucial guidelines for line numbers:
   - The content regarding line numbers MUST strictly follow the format "Replace lines: {{start_line}}-{{end_line}}". Do not be lazy!
   - The range {{start_line}}-{{end_line}} is INCLUSIVE. Both start_line and end_line are included in the replacement.
   - Count EVERY line, including empty lines and comments lines, comments. Do not be lazy!
   - For single-line changes, use the same number for start and end lines.
   - For multi-line changes, ensure the range covers ALL affected lines, from the very first to the very last.
   - Double-check that your line numbers align perfectly with the original code structure.

5. Final check:
   - Review all suggestions, ensuring each line number is correct, especially the start_line and end_line.
   - Confirm that no unrelated code is accidentally modified or deleted.
   - Verify that the start_line and end_line correctly include all intended lines for replacement.
   - Perform a final alignment check to ensure your line numbers haven't shifted, especially the start_line.
   - Double-check that your line numbers align perfectly with the original code structure.
   - Do not show the full content after these modifications.

Remember: Accurate line numbers are CRITICAL. The range start_line to end_line must include ALL lines to be replaced,
 from the very first to the very last. Double-check every range before finalizing your response, paying special attention
 to the start_line to ensure it hasn't shifted down. Ensure that your line numbers perfectly match the original code structure without any overall shift."
  )

(defun gpt-copilot (prompt)
  (interactive (list (read-string "Ask GPT: " nil gptel-lookup--history)))
  (when (string= prompt "") (user-error "A prompt is required."))
  prompt)

(defun gpt-copilot-wrapper ()
  (call-interactively #'gpt-copilot))

(defun gptel-rewrite-and-replace (bounds &optional directive)
  (interactive
   (list
    (cond
     ((use-region-p) (cons (region-beginning) (region-end)))
     ((derived-mode-p 'text-mode)
      (list (bounds-of-thing-at-point 'sentence)))
     (t (cons (line-beginning-position) (line-end-position))))
    (and current-prefix-arg
         (read-string "ChatGPT Directive: "
                      "You are a prose editor. Rewrite my prompt more professionally."))))
  (gptel-request
      (buffer-substring-no-properties (car bounds) (cdr bounds)) ;the prompt

    :system (or directive "You are a prose editor. Rewrite my prompt more professionally.")
    :buffer (current-buffer)
    :context (cons (set-marker (make-marker) (car bounds))
                   (set-marker (make-marker) (cdr bounds)))
    :callback
    (lambda (response info)
      (if (not response)
          (message "ChatGPT response failed with: %s" (plist-get info :status))
        (let* ((bounds (plist-get info :context))
               (beg (car bounds))
               (end (cdr bounds))
               (buf (plist-get info :buffer)))
          (with-current-buffer buf
            (save-excursion
              (goto-char beg)
              (kill-region beg end)
              (insert response)
              (set-marker beg nil)
              (set-marker end nil)
              (message "Rewrote line. Original line saved to kill-ring."))))))))

(defun berge-rewrite-region (bounds &optional directive)
  (interactive
   (list
    (cond
     ((use-region-p) (cons (region-beginning) (region-end)))
     ((derived-mode-p 'text-mode)
      (list (bounds-of-thing-at-point 'sentence)))
     (t (cons (line-beginning-position) (line-end-position))))
    ))

  (setq gptel--system-message
        (format "For this code in \"%s\", send me the code for this request: %s"
                (symbol-name major-mode)
                (gpt-copilot-wrapper)))

  (gptel-request
      (buffer-substring-no-properties (car bounds) (cdr bounds)) ;the prompt

    :buffer (current-buffer)
    :context (cons (set-marker (make-marker) (car bounds))
                   (set-marker (make-marker) (cdr bounds)))
    :callback
    (lambda (response info)
      (if (not response)
          (message "GPT response failed with: %s" (plist-get info :status))
        (let* ((bounds (plist-get info :context))
               (beg (car bounds))
               (end (cdr bounds))
               (buf (plist-get info :buffer)))
          (with-current-buffer buf
            (save-excursion
              (goto-char beg)
              (kill-region beg end)
              (let ((content (if (string-match "```\n\\(\\(?:.\\|\n\\)*?\\)\n```" response)
                                 (match-string 1 response)
                               response)))
                (insert content))
              (set-marker beg nil)
              (set-marker end nil)
              (message "Rewrote line. Original line saved to kill-ring."))))))))

(defun gpt-copilot-setup ()
  "Set up the coding assistant layout."
  (interactive)
  (delete-other-windows)
  (let ((main-buffer (current-buffer)))
    (setq gpt-copilot-chat-buffer
          (get-buffer-create "*Coding Assistant*"))
    (with-current-buffer gpt-copilot-chat-buffer
      (unless (eq major-mode 'gptel-mode)
        (gptel-mode)))
    (split-window-right)
    (set-window-buffer (selected-window) main-buffer)
    (other-window 1)
    (set-window-buffer (selected-window) gpt-copilot-chat-buffer)))

(defun gptel-copilot-query (user-query)
  "Send a query to the GPTel Copilot from the current code buffer."
  (interactive "sEnter your query: ")
  (unless (buffer-live-p gptel-copilot-chat-buffer)
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
    (with-current-buffer gptel-copilot-chat-buffer
      (goto-char (point-max))
      (insert "\nUser: " user-query "\n\nAssistant: ")
      (gptel-request
          full-query
        :callback #'gptel-copilot-handle-response
        :buffer code-buffer
        :position (point-marker)))))

(defun gptel-copilot-handle-response (response info)
  "Handle the response from the GPTel Copilot."
  (when response
    (let ((code-buffer (plist-get info :buffer))
          (chat-buffer gptel-copilot-chat-buffer))
      (with-current-buffer chat-buffer
        (goto-char (point-max))
        (insert response "\n\n"))
      (with-current-buffer code-buffer
        (let ((temp-buffer (generate-new-buffer "*GPTel Copilot Suggestion*")))
          (with-current-buffer temp-buffer
            (insert response)
            (diff-mode))
          (ediff-buffers (current-buffer) temp-buffer))))))

(defcustom gpt-copilot-model gptel-model
  "The model to use for the coding assistant."
  :type 'string
  :group 'gpt-copilot)

(defcustom gpt-copilot-temperature gptel-temperature
  "The temperature setting for the coding assistant."
  :type 'number
  :group 'gpt-copilot)

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

;; TODO bug, once we replace a region with fewer or more lines, the line numbers get thrown off in the other suggested changes

(provide 'gptel-copilot)
