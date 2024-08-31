;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel-copilot)
(require 'gptel)

(setq example-response
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

(ert-deftest extract-changes-test ()
  (let* ((response (gptel-copilot-extract-changes example-response))
	 (explanations (plist-get response :explanations))
	 (changes (plist-get response :changes)))
    (should (equal changes
		   '((:start 1 :end 9
			     :code "package main\nimport \"fmt\"\nfunc main() {\nfmt.Println(\"hello world\")\n}\n")
		     (:start 10 :end 12
			     :code "mkdir hello-world\n./hello_world\n"))))

    (should (equal explanations
		   '("Certainly! Here are some code changes: \n"
		     "1st Code Change:\n\nThis is a hello world function\n"
		     "2nd Code Change:\n\nThese code changes will run the unit test")))))

;; TODO test start and end
(ert-deftest gptel-copilot-apply-changes-multiple-changes ()
  (let ((test-buffer (generate-new-buffer "*test-buffer*"))
	(original-content "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6\nLine 7\nLine 8\nLine 9\nLine 10\nLine 11\nLine 12\n")
	;; Test both newline and no newline after changes
	(changes '((:start 3 :end 5 :code "New Line 3\nNew Line 4\n")
		   (:start 7 :end 10 :code "New Line 7\nNew Line 8\nNew Line 9\nNew Line 10\nNew Line 11"))))))
