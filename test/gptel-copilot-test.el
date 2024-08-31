;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel-copilot)
(require 'gptel)

(setq-local example-response
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
		    "mkdir hello-world\n"
		    "./hello_world\n"
		    "```\n"
		    "These code changes will run the unit test"))

(setq-local test-lines
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

(ert-deftest extract-changes-test ()
  (let* ((response (gptel-copilot-extract-changes example-response))
	 (explanations (plist-get response :explanations))
	 (changes (plist-get response :changes)))
    (should (equal changes
		   '((:start 1 :end 9
			     :code "package main\nimport \"fmt\"\nfunc main() {\n
				    fmt.Println(\"hello world\")\n}\n")
		     (:start 10 :end 12
			     :code "mkdir hello-world\n./hello_world\n"))))

    (should (equal explanations
		   '("Certainly! Here are some code changes: \n"
		     "1st Code Change:\n\nThis is a hello world function\n"
		     "2nd Code Change:\n\nThese code changes will run the unit test")))))

(ert-deftest gptel-copilot-apply-changes-multiple-changes ()
  "Test that the offset is properly setup when applying multiple changes"
  (let ((test-buffer (generate-new-buffer "*test-buffer*"))
	;; Test both newline and no newline after changes
	((expected-result
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
		  ">>>>>>> test-backend\n"))

	 (changes '((:start 3 :end 5 :code "New Line 3\nNew Line 4\n")
		    (:start 7 :end 10 :code
			    "New Line 7\nNew Line 8\nNew Line 9\nNew Line 10\nNew Line 11")))
	 (unwind-protect
	     (progn
	       (with-current-buffer test-buffer
		 (insert original-content)
		 (setq-local gptel-backend test-backend))

	       (gptel-copilot-apply-changes test-buffer changes)

	       (with-current-buffer test-buffer
		 (should (string= (buffer-string) expected-result))
		 (should
		  (string= (gptel-copilot-))))))))))

(ert-deftest gptel-copilot-apply-changes-to-line-1 ()
  (let ((test-buffer (generate-new-buffer "*test-buffer*"))
	;; Test both newline and no newline after changes
	(changes '((:start 1 :end 1 :code "New Line 1"))))))
