;; TODO easily save and reload the AI memory - context for different projects
;; TODO copy contexts of those cursor prompts into an initial context setup
;; TODO test with org

(require 'cl-generic)
(require 'gptel)


(defgroup gpt-copilot nil
  "Apply code changes using gptel"
  :group 'hypermedia)

(defcustom gpt-copilot-window-size 0.33
  "Size of the GPT Copilot chat window as a fraction of the frame.
Must be a number between 0 and 1, exclusive."
  :type 'float
  :group 'gpt-copilot
  :set (lambda (symbol value)
	 (if (and (numberp value)
		  (< 0 value 1))
	     (set-default symbol value)
	   (user-error "gpt-copilot-window-size must be a number between 0 and 1, exclusive"))))

(defcustom gpt-copilot-window-style 'vertical
  "Specify the orientation. It can be \='horizontal, '\=vertical, or nil."
  :type '(choice (const :tag "Horizontal" horizontal)
		 (const :tag "Vertical" vertical)
		 (const :tag "None" nil)))

(defvar gpt-copilot--chat-buffer nil)

(defvar gpt-base-prompt
  (concat
   ;; The prompt is originally from avante.nvim:
   ;; https://github.com/yetone/avante.nvim/blob/main/lua/avante/llm.lua
   "Your primary task is to suggest code modifications with precise line number ranges. Follow these instructions meticulously:\n"

   "1. Carefully analyze the original code, paying close attention to its structure and line numbers. Line numbers start from 1 and include ALL lines, even empty ones.\n"

   "2. When suggesting modifications:\n"
   "a. Use the language in the question to reply. If there are non-English parts in the question, use the language of those parts.\n"
   "b. Explain why the change is necessary or beneficial.\n"
   "c. If an image is provided, make sure to use the image in conjunction with the code snippet.\n"
   "d. Provide the exact code snippet to be replaced using this format:\n"

   "Replace lines: {{start_line}}-{{end_line}}\n"
   ;; We don't need the language here but if we don't specify it then some LLMs might provide it and some might not.
   ;; So it's better to mandate it and filter it later
   "```{{language}}\n"
   "{{suggested_code}}\n"
   "```\n"
   "{{Explanation of the changes}}"

   "3. Crucial guidelines for suggested code snippets:\n"
   "- Only apply the change(s) suggested by the most recent assistant message (before your generation).\n"
   "- Do not make any unrelated changes to the code.\n"
   "- Produce a valid full rewrite of the entire original file without skipping any lines. Do not be lazy!\n"
   "- Do not arbitrarily delete pre-existing comments/empty Lines.\n"
   "- Do not omit large parts of the original file for no reason.\n"
   "- Do not omit any needed changes from the requisite messages/code blocks.\n"
   "- If there is a clicked code block, bias towards just applying that (and applying other changes implied).\n"
   "- Please keep your suggested code changes minimal, and do not include irrelevant lines in the code snippet.\n"
   "- Maintain the SAME indentation in the returned code as in the source code\n"

   "4. Crucial guidelines for line numbers:\n"
   "- The content regarding line numbers MUST strictly follow the format Replace lines: {{start_line}}-{{end_line}}. Do not be lazy!\n"
   "- The range {{start_line}}-{{end_line}} is INCLUSIVE. Both start_line and end_line are included in the replacement.\n"
   "- Count EVERY line, including empty lines and comments lines, comments. Do not be lazy!\n"
   "- For single-line changes, use the same number for start and end lines.\n"
   "- For multi-line changes, ensure the range covers ALL affected lines, from the very first to the very last.\n"
   "- Double-check that your line numbers align perfectly with the original code structure.\n"

   "5. Final check:\n"
   "- Review all suggestions, ensuring each line number is correct, especially the start_line and end_line.\n"
   "- Confirm that no unrelated code is accidentally modified or deleted.\n"
   "- Verify that the start_line and end_line correctly include all intended lines for replacement.\n"
   "- Perform a final alignment check to ensure your line numbers haven't shifted, especially the start_line.\n"
   "- Double-check that your line numbers align perfectly with the original code structure.\n"
   "- Do not show the full content after these modifications.\n"

   "Remember: Accurate line numbers are CRITICAL. The range start_line to end_line must include ALL lines to be replaced, from the very first to the very last. Double-check every range before finalizing your response, paying special attention to the start_line to ensure it hasn't shifted down. Ensure that your line numbers perfectly match the original code structure without any overall shift.\n"
   ))


(defun gpt-copilot-toggle-window ()
  (interactive)
  (if (and (buffer-live-p gpt-copilot--chat-buffer)
	   (get-buffer-window gpt-copilot--chat-buffer))
      (delete-window (get-buffer-window gpt-copilot--chat-buffer))

    (gpt-copilot-setup-windows)))


;; TODO nil doesn't work because of the gptel-command
;; nil should have the var open in the background
(defun gpt-copilot-setup-windows ()
  "Set up the coding assistant layout with the chat window."
  (unless (buffer-live-p gpt-copilot--chat-buffer)
    (setq gpt-copilot--chat-buffer
	  (gptel "*Gptel-Copilot*")))

  (when gpt-copilot-window-style
    (delete-other-windows)

    (let* ((main-buffer (current-buffer))
	   (main-window (selected-window))
	   (split-size (floor (* (if (eq gpt-copilot-window-style 'vertical)
				     (frame-width)
				   (frame-height))
				 (- 1 gpt-copilot-window-size)))))
      (with-current-buffer gpt-copilot--chat-buffer)
      (if (eq gpt-copilot-window-style 'vertical)
	  (split-window-right split-size)
	(split-window-below split-size))
      (set-window-buffer main-window main-buffer)
      (other-window 1)
      (set-window-buffer (selected-window) gpt-copilot--chat-buffer))))


;; TODO test in the chat buffer
;; TODO instead of adding user-query to the full-query, it should be added to the
;; Chat buffer which is then sent to the request
(defun gptel-copilot-query (user-query)
  "Send a query to the GPTel Copilot from the current buffer."
  (interactive "sUser Query: ")
  ;; TODO nil at the start
  (unless (buffer-live-p gpt-copilot--chat-buffer)
    (gpt-copilot-setup-windows))

  (let* ((code-buffer (if (eq (current-buffer) gpt-copilot--chat-buffer)
			  (window-buffer (next-window))
			(current-buffer)))
	 (prompt (buffer-substring-no-properties
		  (region-beginning) (region-end)))

	 (start-line (with-current-buffer code-buffer
		       (if (use-region-p)
			   (line-number-at-pos (region-beginning))
			 1)))

	 (end-line (with-current-buffer code-buffer
		     (if (use-region-p)
			 (line-number-at-pos (region-end))
		       (line-number-at-pos (point-max)))))

	 (selected-code (with-current-buffer code-buffer
			  (if (use-region-p)
			      (buffer-substring-no-properties (region-beginning) (region-end))
			    (buffer-substring-no-properties (point-min) (point-max)))))

	 (file-type (with-current-buffer code-buffer
		      (symbol-name major-mode)))

	 (full-query (format "\n\nFile type: %s\nLine range: %d-%d\n\nCode:\n%s\n\n%s"
			     file-type
			     start-line
			     end-line
			     selected-code
			     user-query)))

    (with-current-buffer gpt-copilot--chat-buffer
      (goto-char (point-max))
      (insert user-query "\n")
      (gptel-request
	  full-query
	:system gpt-base-prompt
	:buffer gpt-copilot--chat-buffer
	:callback #'gptel-copilot-handle-response))))


;; TODO find out if it shows the first explanation
(defun gptel-copilot-handle-response (response info)
  "Handle the response from the GPTel Copilot, applying changes in git merge format."
  (when response
    (let* ((code-buffer (if (eq (current-buffer) gpt-copilot--chat-buffer)
			    (window-buffer (next-window))
			  (current-buffer)))
	   (extracted-data (gptel-copilot-extract-changes response))
	   (changes (plist-get extracted-data :changes))
	   (explanations (plist-get extracted-data :explanations)))

      ;; Apply changes to code buffer in git merge format
      (when changes
	(gptel-copilot-apply-changes code-buffer changes))

      ;; Insert explanations into chat buffer
      (with-current-buffer gpt-copilot--chat-buffer
	(dolist (explanation explanations)
	  (let ((explanation-info (list :buffer gpt-copilot--chat-buffer
					:position (point-max-marker)
					:in-place t)))
	    (gptel--insert-response (string-trim explanation) explanation-info)))

	;; Add a message in the chat buffer indicating that changes were applied
	(gptel--sanitize-model)
	;; (gptel--insert-response
	;;  (format "%d change(s) have been applied to the code buffer in git merge format." (length changes))
	;;  patch-message-info)
	(gptel--update-status " Waiting..." 'warning)))))


;; TODO unit test
(defun gptel-copilot-extract-changes (response)
  "Extract code changes from the ``` blocks, and explanations. Explanations will be of the format:
{Initial explanation}

1st Code Change:
{Code Change}

2nd Code Change:
{Code Change}
"
  (let ((changes '())
	(explanations '())
	(start 0)
	(change-count 0)
	(code-block-regex
	 "Replace lines: \\([0-9]+\\)-\\([0-9]+\\)\n```\\(?:[[:alpha:]-]+\\)?\n\\(\\(?:.\\|\n\\)*?\\)```"))
    (while (string-match code-block-regex response start)
      (let ((change-start (string-to-number (match-string 1 response)))
	    (change-end (string-to-number (match-string 2 response)))
	    (code (match-string 3 response))
	    (explanation-text (substring response start (match-beginning 0))))

	;; TODO make this different for the initial explanation
	(when (not (string-empty-p explanation-text))
	  (setq change-count (1+ change-count))
	  (push (format "%s Code Change:\n%s\n\n"
			(gptel-copilot--ordinal change-count)
			explanation-text)
		explanations))

	(push (list :start change-start
		    :end change-end
		    :code code)
	      changes)

	;; Update start index in the response string
	(setq start (match-end 0))))

    ;; Add any remaining text as the last explanation
    (let ((remaining-text (substring response start)))
      (when (not (string-empty-p remaining-text))
	(setq change-count (1+ change-count))
	(push (concat (format "%s change:\n" (gptel-copilot--ordinal change-count))
		      remaining-text)
	      explanations)))
    (list :explanations (nreverse explanations)
	  :changes (nreverse changes))))


;; TODO unit test
(defun gptel-copilot-apply-changes (buffer changes)
  "Apply changes to buffer in a git merge format.
We need to keep track of an offset of line numbers. For example, if we
replace a block of six lines with three lines, then the line numbers provided by the LLM
will need to be offset by -3. Similarly, the >>>>>>>, <<<<<<<, and ======= lines added
will offset the LLM line numbers by 3"
  ;; TODO code buffer
  (with-current-buffer buffer
    (save-excursion
      (let ((line-offset 0))
	(dolist (change changes)
	  (let* ((start (+ (plist-get change :start) line-offset))
		 (end (+ (plist-get change :end) line-offset))
		 (new-code (plist-get change :code))
		 (old-lines (- end start -1))
		 (new-lines (length (split-string new-code "\n")))
		 (merge-line-count 3))  ; Offsets from the three merge strings
	    (goto-char (point-min))
	    (forward-line (1- start))
	    (insert "<<<<<<< HEAD\n")
	    (let ((beg (point)))
	      (forward-line old-lines)
	      (let ((old-content (buffer-substring beg (point))))
		(delete-region beg (point))
		(insert old-content)))
	    (insert "=======\n")
	    (insert new-code)
	    (unless (string-suffix-p "\n" new-code)
	      (insert "\n"))
	    (insert (format ">>>>>>> %s\n" (gptel-backend-name gptel-backend)))
	    (setq line-offset (+ line-offset (- new-lines old-lines) merge-line-count))))))))


(defun gptel-copilot--ordinal (n)
  "Convert integer N to its ordinal string representation."
  (let ((suffixes '("th" "st" "nd" "rd" "th" "th" "th" "th" "th" "th")))
    (if (and (> n 10) (< n 14))
	(concat (number-to-string n) "th")
      (concat (number-to-string n)
	      (nth (mod n 10) suffixes)))))


(provide 'gptel-copilot)
