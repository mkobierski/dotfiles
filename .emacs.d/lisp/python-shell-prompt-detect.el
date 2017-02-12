; (MK) Modified to support windows properly
(defun python-shell-prompt-detect ()
  "Detect prompts for the current `python-shell-interpreter'.
When prompts can be retrieved successfully from the
`python-shell-interpreter' run with
`python-shell-interpreter-interactive-arg', returns a list of
three elements, where the first two are input prompts and the
last one is an output prompt.  When no prompts can be detected
and `python-shell-prompt-detect-failure-warning' is non-nil,
shows a warning with instructions to avoid hangs and returns nil.
When `python-shell-prompt-detect-enabled' is nil avoids any
detection and just returns nil."
  (when python-shell-prompt-detect-enabled
    (let* ((process-environment (python-shell-calculate-process-environment))
           (exec-path (python-shell-calculate-exec-path))
           (code (concat
                  "import sys\n"
                  "ps = [getattr(sys, 'ps%s' % i, '') for i in range(1,4)]\n"
                  ;; JSON is built manually for compatibility
                  "ps_json = '\\n[\"%s\", \"%s\", \"%s\"]\\n' % tuple(ps)\n"
                  "print (ps_json)\n"
                  "sys.exit(0)\n"))
           (output
            (with-temp-buffer
              ;; TODO: improve error handling by using
              ;; `condition-case' and displaying the error message to
              ;; the user in the no-prompts warning.
              (ignore-errors
                (let ((code-file (python-shell--save-temp-file code)))
                  ;; Use `process-file' as it is remote-host friendly.
                  (process-file
                   python-shell-interpreter
                   code-file
                   '(t nil)
                   nil
                   python-shell-interpreter-interactive-arg)
                  ;; Try to cleanup
                  (delete-file code-file)))
              (buffer-string)))
           (prompts
            (catch 'prompts
              (dolist (line (split-string output "\n" t))
                (let ((res
                       ;; Check if current line is a valid JSON array
                       (and (string= (substring line 0 2) "[\"")
                            (ignore-errors
                              ;; Return prompts as a list, not vector
                              (append (json-read-from-string line) nil)))))
                  ;; The list must contain 3 strings, where the first
                  ;; is the input prompt, the second is the block
                  ;; prompt and the last one is the output prompt.  The
                  ;; input prompt is the only one that can't be empty.
                  (when (and (= (length res) 3)
                             (cl-every #'stringp res)
                             (not (string= (car res) "")))
                    (throw 'prompts res))))
              nil)))
      prompts)))
