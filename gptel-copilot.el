;; -*- lexical-binding: t; -*-

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

   "Remember: Accurate line numbers are CRITICAL. The range start_line to end_line must include ALL lines to be replaced, from the very first to the very last. Double-check every range before finalizing your response, paying special attention to the start_line to ensure it hasn't shifted down. Ensure that your line numbers perfectly match the original code structure without any overall shift.\n"))


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
	  (gptel "*Gptel Copilot*")))

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


;; TODO instead of adding user-query to the full-query, it should be added to the
;; Chat buffer which is then sent to the request
(defun gptel-copilot-query (user-query)
  "Send a query to the GPTel Copilot from the current buffer or chat buffer."
  (interactive
   (list
    (if (eq (current-buffer) gpt-copilot--chat-buffer)
	nil		; We'll extract the query from the chat buffer
      (read-string "User Query: "))))
  (unless (buffer-live-p gpt-copilot--chat-buffer)
    (gpt-copilot-setup-windows))
  (let* ((in-chat-buffer (eq (current-buffer) gpt-copilot--chat-buffer))
	 (code-buffer (if in-chat-buffer
			  (window-buffer (next-window))
			(current-buffer)))
	 (chat-buffer gpt-copilot--chat-buffer)
	 (extracted-query
	  (when in-chat-buffer
	    (gptel-copilot-parse-user-query chat-buffer)))
	 (final-user-query (or user-query extracted-query
			       (user-error "No query provided")))
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
			     final-user-query)))

    (message (format  "Querying %s..." (gptel-backend-name gptel-backend)))
    (with-current-buffer chat-buffer
      (goto-char (point-max))
      (unless in-chat-buffer
	(insert final-user-query "\n\n"))
      (gptel--update-status " Waiting..." 'warning)
      (gptel-request
	  full-query
	:system gpt-base-prompt
	:buffer chat-buffer
	:callback #'gptel-copilot-handle-response)))
  (gptel--update-status " Ready" 'success))


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
	))))


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
	 "Replace [Ll]ines:? \\([0-9]+\\)-\\([0-9]+\\)\n```\\(?:[[:alpha:]-]+\\)?\n\\(\\(?:.\\|\n\\)*?\\)```"))
    (while (string-match code-block-regex response start)
      (let ((change-start (string-to-number (match-string 1 response)))
	    (change-end (string-to-number (match-string 2 response)))
	    (code (match-string 3 response))
	    (explanation-text (substring response start (match-beginning 0))))
	;; the initial explanation won't be preceded by nth Code Change
	(when (not (string-empty-p explanation-text))
	  (push (if (= 0 change-count)
		    explanation-text  ; For the first explanation, just use the text as is
		  (format "%s Code Change:\n%s"
			  (gptel-copilot--ordinal change-count)
			  explanation-text))
		explanations)
	  (setq change-count (1+ change-count)))
	(push (list :start change-start
		    :end change-end
		    :code code)
	      changes)
	;; Update start index in the response string
	(setq start (match-end 0))))
    ;; Add any remaining text as the last explanation
    (let ((remaining-text (substring response start)))
      (when (not (string-empty-p remaining-text))
	(push (if (= 0 change-count)
		  remaining-text
		(format "%s Code Change:\n%s"
			(gptel-copilot--ordinal change-count)
			remaining-text))
	      explanations)))
    (list :explanations (nreverse explanations)
	  :changes (nreverse changes))))


(defun gptel-copilot-apply-changes (buffer changes)
  "Apply changes to buffer in a git merge format.
We need to keep track of an offset of line numbers. Because the AI gives us line numbers
based on the current buffer, all inserted changes will offset those line numbers. So if
we insert a sequence of lines in addition to the >>>>>>>, =======, <<<<<<< strings for a
change, then the offset of the subsequent inserted lines will need to be offset by
3 (number of merge strings) + the length of the newlines"
  (with-current-buffer buffer
    (let ((offset 0))
      (dolist (change changes)
	(let* ((start (+ (plist-get change :start) offset))
	       (end (+ (plist-get change :end) offset))
	       (new-code (string-trim-right (plist-get change :code)))
	       (old-region-size (- end start))
	       (new-region-size (+ (length (split-string new-code "\n" t)) 3))) ; 3 for merge markers
	  (replace-region-contents
	   (line-beginning-position start)
	   (line-beginning-position end)
	   (lambda ()
	     (format "<<<<<<< HEAD\n%s=======\n%s\n>>>>>>> %s\n"
		     (buffer-substring (line-beginning-position start)
				       (line-beginning-position end))
		     new-code
		     (gptel-backend-name gptel-backend))))
	  (setq offset (+ offset (- new-region-size old-region-size))))))))


;; TODO this could probably be replaced with something already in gptel
(defun gptel-copilot-parse-user-query (buffer)
  "Parse and extract the most recent user query from the buffer.
The query is expected to be after the last '* ' (org-mode) or '### ' (markdown-mode) heading.
Returns nil if no query is found."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (let ((case-fold-search t)
	    (heading-regex (if (derived-mode-p 'org-mode)
			       "^\\* "
			     "^### ")))
	(when (re-search-backward heading-regex nil t)
	  (buffer-substring-no-properties
	   (line-beginning-position 2)  ; Start from next line
	   (point-max)))))))


(defun gptel-copilot--ordinal (n)
  "Convert integer N to its ordinal string representation."
  (let ((suffixes '("th" "st" "nd" "rd" "th" "th" "th" "th" "th" "th")))
    (if (and (> n 10) (< n 14))
	(concat (number-to-string n) "th")
      (concat (number-to-string n)
	      (nth (mod n 10) suffixes)))))


(provide 'gptel-copilot)

(let ((test-buffer (generate-new-buffer "*insert-test*")))
  (unwind-protect
      (progn
	(with-current-buffer test-buffer
	  (insert "First line\nSecond line\nThird line")
	  (goto-char (point-min))
	  (forward-line 1)
	  (insert "Inserted text: ")
	  (message (buffer-string)))
	)
    (kill-buffer test-buffer)))
