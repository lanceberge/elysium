;;; test-elysium.el --- elysium tests-*- lexical-binding: t; package-lint-main-file: "../elysium.el"; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Lance Bergeron <bergeron.lance6@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Tests
;;

;;; Code:
(require 'elysium)
(require 'ert)

(ert-deftest test-extract-changes ()
  (let* ((example-response
	  (concat "Certainly! Here are some code changes: \n"
		  "Replace Lines: 1-9\n"
		  "```go\n"
		  "package main\n"
		  "import \"fmt\"\n"
		  "func main() {\n"
		  "fmt.Println(\"hello world\")\n"
		  "}\n"
		  "```\n"
		  "This is a hello world function\n"
		  "Replace Lines 10-12\n"
		  "```bash\n"
		  "go build hello_world.go\n"
		  "./hello_world\n"
		  "```\n"
		  "These code changes will run the unit test"))
	 (response (elysium-extract-changes example-response))
	 (explanations (plist-get response :explanations))
	 (changes (plist-get response :changes)))
    (should (equal changes
		   '((:start 1 :end 9
			     :code "package main\nimport \"fmt\"\nfunc main() {\nfmt.Println(\"hello world\")\n}\n")
		     (:start 10 :end 12
			     :code "go build hello_world.go\n./hello_world\n"))))

    (should (equal explanations
		   '("Certainly! Here are some code changes: \n"
		     "1st Code Change:\n\nThis is a hello world function\n"
		     "2nd Code Change:\n\nThese code changes will run the unit test")))))

(ert-deftest test-apply-multiple-changes ()
  "Test that the offset is properly setup when applying multiple changes"
  (let ((test-buffer (generate-new-buffer "*test-buffer*"))
	(test-backend (gptel--make-backend
		       :name "test-backend"))
	(test-lines
	 (concat "Line 1\n"
		 "Line 2\n"
		 "Line 3\n"
		 "Line 4\n"
		 "Line 5\n"
		 "Line 6\n"
		 "Line 7\n"
		 "Line 8\n"
		 "Line 9\n"
		 "Line 10\n"
		 "Line 11\n"
		 "Line 12\n"))
	(expected-result
	 (concat "Line 1\n"
		 "Line 2\n"
		 "<<<<<<< HEAD\n"
		 "Line 3\n"
		 "Line 4\n"
		 "Line 5\n"
		 "=======\n"
		 "New Line 3\n"
		 "New Line 4\n"
		 ">>>>>>> test-backend\n"
		 "Line 6\n"
		 "<<<<<<< HEAD\n"
		 "Line 7\n"
		 "Line 8\n"
		 "Line 9\n"
		 "Line 10\n"
		 "=======\n"
		 "New Line 7\n"
		 "New Line 8\n"
		 "New Line 9\n"
		 "New Line 10\n"
		 "New Line 11\n"
		 ">>>>>>> test-backend\n"
		 "Line 11\n"
		 "Line 12\n"))

	;; Test both newline and no newline after changes
	(changes '((:start 3 :end 5 :code "New Line 3\nNew Line 4\n")
		   (:start 7 :end 10 :code
			   "New Line 7\nNew Line 8\nNew Line 9\nNew Line 10\nNew Line 11"))))
    (unwind-protect
	(progn
	  (with-current-buffer test-buffer
	    (insert test-lines)
	    (setq-local gptel-backend test-backend))

	  (elysium-apply-code-changes test-buffer changes)

	  (with-current-buffer test-buffer
	    (should (string= (buffer-string) expected-result)))))))

(provide 'test-elysium)

;;; test-elysium ends here
