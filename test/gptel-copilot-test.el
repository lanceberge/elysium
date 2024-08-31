;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel-copilot)
(require 'cl-generic)

(defvar example-response
  (concat "Certainly! Here are some code changes: \n"
	  "Replace Lines: 1-9\n"
	  "```go\n"
	  "package main\n"
	  "import \"fmt\"\n"
	  "func main() {\n"
	  "fmt.Println(\"hello world\")\n"
	  "}\n\n"
	  "This is a hello world function\n"
	  "Replace Lines 10-12\n"
	  "```bash\n"
	  "mkdir hello-world\n"
	  "./hello_world\n"
	  "These code changes will ..."))

(ert-deftest extract-changes-test ()
  (let* ((response (gptel-copilot-extract-changes example-response))
	 (explanations (plist-get response :explanations))
	 (changes (plist-get response :changes)))
    (should (equal changes
		   '((:start 1 :end 9
			     :code "package main\nimport \"fmt\"\nfunc main() {\nfmt.Println(\"hello world\")\n}\n\n")
		     (:start 10 :end 12
			     :code "mkdir hello-world\n./hello_world\n"))))

    (should (equal explanations
		   '("Certainly! Here are some code changes: \n"
		     "This is a hello world function\n"
		     "These code changes will ...")))))
