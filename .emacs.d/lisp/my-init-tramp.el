; Workaround for using tramp with cygwin
; 
; http://www.emacswiki.org/emacs/SshWithNTEmacs
;
; We want tramp to use fakecygpty when connecting via ssh
; because the ssh client within cygwin does not work properly on windows
(defun my-init-tramp ()
  (eval-after-load "tramp"
    '(progn
       (add-to-list 'tramp-methods
                    (mapcar
                     (lambda (x)
                       (cond
                        ((equal x "sshx") "cygssh")
                        ((eq (car x) 'tramp-login-program) (list 'tramp-login-program "fakecygpty ssh"))
                        (t x)))
                     (assoc "sshx" tramp-methods)))
       (setq tramp-default-method "cygssh"))))
