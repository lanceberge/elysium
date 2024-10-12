;;; elysium.el --- Automatically apply LLM-created code-suggestions -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Lance Bergeron

;; Author: Lance Bergeron <bergeron.lance6@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))
;; URL: https://github.com/lanceberge/elysium/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package extends on gptel.el.  It uses that package to generate code
;; suggestions based on the user's request.  Those code suggestions will then
;; automatically be applied to the buffer in the format of a git merge.

;;; Code:
(require 'gptel)
(require 'smerge-mode)

(defgroup elysium nil
  "Apply code changes using gptel."
  :group 'hypermedia)

(defcustom elysium-window-size 0.33
  "Size of the elysium chat window as a fraction of the frame.
Must be a number between 0 and 1, exclusive."
  :type 'float
  :group 'elysium
  :set (lambda (symbol value)
         (if (and (numberp value)
                  (< 0 value 1))
             (set-default symbol value)
           (user-error "Elysium-window-size must be a number between 0 and 1, exclusive"))))

(defcustom elysium-window-style 'vertical
  "Specify the orientation.  It can be \='horizontal, '\=vertical, or nil."
  :type '(choice (const :tag "Horizontal" horizontal)
                 (const :tag "Vertical" vertical)
                 (const :tag "None" nil)))

(defvar elysium--chat-buffer nil)

(defvar elysium-base-prompt
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

(defun elysium-toggle-window ()
  "Toggle the elysium chat window."
  (interactive)
  (if (and (buffer-live-p elysium--chat-buffer)
           (get-buffer-window elysium--chat-buffer))
      (delete-window (get-buffer-window elysium--chat-buffer))

    (elysium-setup-windows)))

(defun elysium-setup-windows ()
  "Set up the coding assistant layout with the chat window."
  (unless (buffer-live-p elysium--chat-buffer)
    (setq elysium--chat-buffer
          (gptel "*elysium*")))

  (when elysium-window-style
    (delete-other-windows)

    (let* ((main-buffer (current-buffer))
           (main-window (selected-window))
           (split-size (floor (* (if (eq elysium-window-style 'vertical)
                                     (frame-width)
                                   (frame-height))
                                 (- 1 elysium-window-size)))))
      (with-current-buffer elysium--chat-buffer)
      (if (eq elysium-window-style 'vertical)
          (split-window-right split-size)
        (split-window-below split-size))
      (set-window-buffer main-window main-buffer)
      (other-window 1)
      (set-window-buffer (selected-window) elysium--chat-buffer))))

;; TODO instead of adding user-query to the full-query, it should be added to the
;; Chat buffer which is then sent to the request
(defun elysium-query (user-query)
  "Send USER-QUERY to elysium from the current buffer or chat buffer."
  (interactive
   (list
    (if (eq (current-buffer) elysium--chat-buffer)
        nil ; We'll extract the query from the chat buffer
      (read-string "User Query: "))))
  (unless (buffer-live-p elysium--chat-buffer)
    (elysium-setup-windows))
  (let* ((in-chat-buffer (eq (current-buffer) elysium--chat-buffer))
         (code-buffer (if in-chat-buffer
                          (window-buffer (next-window))
                        (current-buffer)))
         (chat-buffer elysium--chat-buffer)
         (extracted-query
          (when in-chat-buffer
            (elysium-parse-user-query chat-buffer)))
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

    (gptel--update-status " Waiting..." 'warning)
    (message "Querying %s..." (gptel-backend-name gptel-backend))
    (save-excursion
      (with-current-buffer chat-buffer
        (goto-char (point-max))
        (unless in-chat-buffer
          (insert final-user-query))
        (insert "\n\n")))

    (gptel-request full-query
      :system elysium-base-prompt
      :buffer chat-buffer
      :callback (apply-partially #'elysium-handle-response code-buffer))))

(defun elysium-keep-all-suggested-changes ()
  "Keep all of the LLM suggestions."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (funcall #'smerge-keep-lower))
    (while (ignore-errors (not (smerge-next)))
      (funcall #'smerge-keep-lower))))

(defun elysium-discard-all-suggested-changes ()
  "Discard all of the LLM suggestions."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (funcall #'smerge-keep-upper))
    (while (ignore-errors (not (smerge-next)))
      (funcall #'smerge-keep-upper))))

(defun elysium-handle-response (code-buffer response info)
  "Handle the RESPONSE from gptel.
The changes will be applied in a git merge format.  INFO is passed into
this function from the `gptel-request' function."
  (when response
    (let* ((extracted-data (elysium-extract-changes response))
           (changes (plist-get extracted-data :changes))
           (explanations (plist-get extracted-data :explanations)))

      (when changes
        (elysium-apply-code-changes code-buffer changes))

      ;; Insert explanations into chat buffer
      (with-current-buffer elysium--chat-buffer
        (dolist (explanation explanations)
          (let ((explanation-info (list :buffer (plist-get info :buffer)
                                        :position (point-max-marker)
                                        :in-place t)))
            (gptel--insert-response (string-trim explanation) explanation-info)))

        (gptel--sanitize-model)
        (gptel--update-status " Ready" 'success)))))

(defun elysium-extract-changes (response)
  "Extract the code-changes and explanations from RESPONSE.
Explanations will be of the format:
{Initial explanation}

1st Code Change:
{Code Change}

2nd Code Change:
{Code Change}"
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
                          (elysium--ordinal change-count)
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
                        (elysium--ordinal change-count)
                        remaining-text))
              explanations)))
    (list :explanations (nreverse explanations)
          :changes (nreverse changes))))

(defun elysium-apply-code-changes (buffer code-changes)
  "Apply CODE-CHANGES to BUFFER in a git merge format.
We need to keep track of an offset of line numbers.  Because the AI gives us
line numbers based on the current buffer, all inserted code-changes will offset
those line numbers.  So if we insert a sequence of lines in addition to the
>>>>>>>, =======, <<<<<<< strings for a change, then the offset of the
subsequent inserted lines will need to be offset by
3 (number of merge strings) + the length of the newlines"
  (with-current-buffer buffer
    (save-excursion
      (let ((offset 0))
        (dolist (change code-changes)
          (let* ((start (plist-get change :start))
                 (end (plist-get change :end))
                 (new-code (string-trim-right (plist-get change :code)))
                 (new-lines (split-string new-code "\n" t)))
            (goto-char (point-min))
            (forward-line (1- (+ start offset)))
            (insert "<<<<<<< HEAD\n")

            ;; Skip forward over the previous code
            (forward-line (+ 1 (- end start)))
            (insert (format "=======\n%s\n>>>>>>> %s\n"
                            new-code
                            (gptel-backend-name gptel-backend)))
            (setq offset (+ offset 3 (length new-lines)))))))))

;; TODO this could probably be replaced with something already in gptel
(defun elysium-parse-user-query (buffer)
  "Parse and extract the most recent user query from BUFFER.
The query is expected to be after the last '* ' (org-mode) or
 '### ' (markdown-mode) heading.  Returns nil if no query is found."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (let ((case-fold-search t)
            (heading-regex (if (derived-mode-p 'org-mode)
                               "^\\*\\*\\* "
                             "^### ")))
        (when (re-search-backward heading-regex nil t)
          (let ((query-text (buffer-substring-no-properties (point) (point-max))))
            (string-trim query-text)))))))

(defun elysium--ordinal (n)
  "Convert integer N to its ordinal string representation."
  (let ((suffixes '("th" "st" "nd" "rd" "th" "th" "th" "th" "th" "th")))
    (if (and (> n 10) (< n 14))
        (concat (number-to-string n) "th")
      (concat (number-to-string n)
              (nth (mod n 10) suffixes)))))

(provide 'elysium)

;;; elysium.el ends here
