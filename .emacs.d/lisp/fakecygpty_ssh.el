; Use fakecygpty to oversome some issues with shell io when using
; ssh within emacs on windows.
  (defun ssh (hostname &optional flags)
    "Start an SSH session in a shell window."
    (interactive "sSSH to host: ")
    (let ((buf (concat "*SSH:" hostname "*")))
      (if (and (get-buffer buf) (get-buffer-process buf))
          (switch-to-buffer-other-window buf)
        (async-shell-command (concat "fakecygpty ssh " flags (when flags " ") hostname) buf))))
