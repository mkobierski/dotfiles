; Turn off the splash screen and remove menus
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

; Set up server start, so we can connect additional instances
; of emacs to this frame
(server-start)

; Set up our package servers
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                          ("org" . "http://orgmode.org/elpa/")))
(when (< emacs-major-version 24)
  ;; for important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" ."http://elpa.gnu.org/packages/")))
(package-initialize)

; Load all libraries in emacs.d/lisp
(add-to-list 'load-path "~/.emacs.d/lisp")

; Configure cygwin to be used for the shell using cygwin-mount
(setenv "PATH" (concat "c:/cygwin64/bin/;" (getenv "PATH")))
(setq exec-path (cons "c:/cygwin64/bin/" exec-path))
(require 'cygwin-mount)
(cygwin-mount-activate)

(load-library "fill-column-indicator")
(load-library "shell-quote-argument.el")
(load-library "imenu+")
(load-library "python-shell-prompt-detect")
(load-library "fakecygpty_ssh")
(load-library "my-init-tramp.el")
(load-library "my-macros.el")


; Set up emacs-w3m
; (require 'w3m-load)


; PACKAGES
; --------
; Linum
(require 'linum-relative)
(global-linum-mode)

; Fill column indicator
(require 'fill-column-indicator)
(setq-default fill-column 80)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

; IDO mode
(require 'idomenu)
(ido-mode 1)
(setq ido-enable-flex-matching t)

; Visible mark
(require 'visible-mark)
(global-visible-mark-mode 1)
(setq visible-mark-max 2)
(setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))

; IdoMenu
(require 'idomenu)

; Multiple cursors
(require 'multiple-cursors)


; Show columns
(define-globalized-minor-mode global-column-number-mode
  column-number-mode
  (lambda () (column-number-mode)))
(global-column-number-mode)

; Interactive shells
; ------------------
; Bash
(add-hook 'comint-output-filter-functions
    'shell-strip-ctrl-m nil t)
(add-hook 'comint-output-filter-functions
;          'comint-watch-for-password-prompt nil t)
          'comint-watch-for-password-prompt)
(setq explicit-shell-file-name "bash")
(add-hook 'shell-mode-hook
          (lambda () (smartscan-mode 0)))
(add-hook 'shell-mode-hook 'turn-off-fci-mode)
(add-hook 'shell-mode-hook
          (lambda () (set-buffer-process-coding-system 'utf-8 'unix)))

; Python
(add-hook 'inferior-python-mode-hook
          (lambda () (smartscan-mode 0)))
(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "C-c C-b") 'python-shell-send-buffer)))
(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "C-c C-c") 'comment-region)))
(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "C-c C-w") 'whitespace-mode)))
(add-hook 'python-mode-hook
          (lambda() (local-set-key  (kbd "C-c C-.") 'my-tags-apropos)))
(add-hook 'python-mode-hook
          (lambda() (local-set-key  (kbd "C-c C-z") 'recompile)))
(add-hook 'python-mode-hook
          (lambda() (local-set-key  (kbd "C-c C-d") 'python-shell-switch-to-shell)))

;; For subprocesses invoked via the shell
;; (e.g., "shell -c command")
(setq shell-file-name explicit-shell-file-name)

; These may not be required anymore...
;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;(add-to-list 'comint-coutput-filter-functions 'ansi-color-process-output)

; Python
(setq-default python-shell-interpreter "c:/python27/python.exe")

; CTags
(setq-default path-to-ctags "/cygdrive/c/usersoftware/ctags")

; Additional hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;
; Custom keybindings
; ------------------
(global-set-key "\C-h\C-f" 'find-function-at-point)
(global-set-key "\C-cl" 'hlt-highlight-symbol)
(global-set-key "\C-cu" 'hlt-unhighlight-symbol)
(global-set-key "\C-z" 'mc/mark-next-word-like-this)
(global-set-key (kbd "S-<f12>") 'mc/mark-next-like-this)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key (kbd "C-`") 'my-push-mark-no-activate)
(global-set-key (kbd "M-`") 'my-jump-to-mark)
(global-set-key (kbd "M-i") 'my-ido-goto-symbol)
(global-set-key (kbd "C-M-z") 'exit-recursive-edit)

(define-key global-map
  [remap exchange-point-and-mark] 'my-exchange-point-and-mark-no-activate)

;
; Editor settings
; ---------------
; Movement
(global-smartscan-mode 1)

; Newline on save
(setq-default require-final-newline 'visit-save)
(setq-default delete-trailing-lines nil)

; Tab settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

; C/C++ code
(defvaralias 'c-basic-offset 'tab-width)
(c-add-style "my-style"
             '((c-offsets-alist
                (cpp-macro . 0)
                (substatement-open . 0)
                (comment-intro . 0)
                (innamespace . 0)
                (label . 0)
                (inher-intro . 0)
                (topmost-intro (my-c-fix-post-define-class-end
                                0))
                (topmost-intro-cont
                 (my-c-lineup-template-function-return
                  my-c-fix-post-define-class-end
                  *))
                (statement-cont
                 (my-c-fix-post-define-class-end
                  +))
                (template-args-cont . +)
                (arglist-intro . +)
                (inline-open my-indent-oneline-definitions)
                (defun-open my-indent-oneline-definitions)
                (arglist-cont-nonempty . *)
                (arglist-cont . /))))

(setq-default c-default-style "my-style")
(add-hook 'c-mode-common-hook 'subword-mode)
(add-hook 'c-mode-common-hook
  (lambda() (local-set-key  (kbd "C-c C-t") 'ff-find-other-file)))
(add-hook 'c-mode-common-hook
  (lambda() (local-set-key  (kbd "C-c C-.") 'my-tags-apropos)))
(add-hook 'c-mode-common-hook
  (lambda() (local-set-key  (kbd "C-c C-z") 'recompile)))


; Disable electric indent
(defun my-disable-electric-indentation()
  "Stop ';', '}', etc. from re-indenting the current line."
  ;(c-toggle-electric-state -1))
  )
(add-hook 'c-mode-common-hook 'my-disable-electric-indentation)

; Function definitions
(defun my-mingw-mode ()
  (interactive)
  (let ((mingw-path "c:/MinGW/bin"))
       (setq exec-path (cons mingw-path exec-path))
       (setenv "PATH" (concat mingw-path ";" (getenv "PATH")))))

(defun my-mingw32-mode ()
  (interactive)
  (let ((mingw-path "c:/mingw32/bin"))
       (setq exec-path (cons mingw-path exec-path))
       (setenv "PATH" (concat mingw-path ";" (getenv "PATH")))))

;
; THEMES
; ------
; Solarized theme
; (add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
; (add-hook 'after-make-frame-functions
;           (lambda (frame)
;             (let ((mode (if (display-graphic-p frame) 'light 'dark)))
;               (set-frame-parameter frame 'background-mode mode)
;               (set-terminal-parameter frame 'background-mode mode))
;             (enable-theme 'solarized)))
; (setq frame-background-mode 'light)

;(load-theme 'solarized t)
;(enable-theme 'solarized)

; Monokai theme - active
(load-theme 'monokai t)

; Themes we consider safe
(setq custom-safe-themes
      '("05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default))

;; ; Things added by customize-variables
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    (quote
;;     ("05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default)))
;;  '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 98 :width normal)))))

; SSH stuff
; ---------
(my-init-tramp)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;'(compilation-environment
 ;  (quote
 ;   ("BOOST_ROOT=[[ path to boost ]]")))
 '(vc-git-program "c:/Program Files (x86)/Git/bin/git.exe"))

; Enable functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)