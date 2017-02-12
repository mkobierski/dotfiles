(defun my-create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (let ((previous-directory default-directory))
    (cd dir-name)
    (unwind-protect
        (shell-command
         (format "%s -e -R ."
                 path-to-ctags))
      (cd previous-directory)))
)

(defun my-replace-line-widths ()
  (interactive)
  (while (search-forward "LineWidths" nil t)
    (setq endl-point nil)
    (save-excursion
      (search-forward "0.5")
      (setq endl-point (point)))
    (replace-string "0.5" "1" nil (point) endl-point nil))
  )

(defun my-push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))


(defun my-jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun my-exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(defun my-ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (my-ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (my-ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(defun my-c-lineup-template-function-return (langelem)
  "Line up the return type of a function with the opening template statement.

v Base-offset
template< typename T >
int foo() {
}

Works with: topmost-intro-cont."
  (save-excursion
    (c-with-syntax-table c++-template-syntax-table
      (beginning-of-line)
      (backward-sexp)
      (let (is<
            istemplate)
        (setq is< (eq (char-after) ?<))
        (backward-sexp)
        (setq istemplate (looking-at "template"))
        (when (and is< istemplate)
            (vector (current-column)))))))

(defun my-back-skip-labels ()
  "Calls backward-sexp so long as we are looking at a C++ label."
  (backward-sexp)
  (when (or (looking-at "public:")
            (looking-at "protected:")
            (looking-at "private:"))
    (my-back-skip-labels)))

(defun my-indent-oneline-definitions (langelem)
  "Indents a oneline definition by basic-offset.

void foo()
<-->{ return 0; }

void foo()
{
    return 0;
}

void foo()
{ return 0;
}

Works with: inline-open, defun-open."
  (save-excursion
    (back-to-indentation)
    (let ((current-line (line-number-at-pos)))
      (forward-sexp)
      (when (and (eq (char-before) ?})
                 (eq (line-number-at-pos) current-line))
        '+))))

(defun my-c-move-up-opening-substatement-brace (langelem)
  "Moves an opening '{' character of if and for statements to the preceeding
line.

Doesn't actually work...

Works with: substatement-open."
  (save-excursion
    (back-to-indentation)
    (let (previous-if-or-for)
      (save-excursion
        (backward-sexp 2)
        (setq previous-if-or-for (or (looking-at "if") (looking-at "for"))))
      (if previous-if-or-for
          (my-c-move-up-opening-brace)))))

(defun my-c-move-up-opening-brace (&optional langelem)
  "Moves an opening brace '{' to the previous line.

Doesn't actually work as expected...

Works with: inline-open."
  (back-to-indentation)
  (if (eq (char-after) ?{)
      (progn
        (c-hungry-delete-backwards)
        (just-one-space))))

(defun my-tags-apropos (regexp)
  "Display list of all tags in tags table REGEXP matches."
  (interactive (find-tag-interactive "Tags apropos (regexp): "))
  (with-output-to-temp-buffer "*Tags List*"
    (princ "Click mouse-2 to follow tags.\n\nTags matching regexp `")
    (tags-with-face 'highlight (princ regexp))
    (princ "':\n\n")
    (save-excursion
      (let ((first-time t))
	(while (visit-tags-table-buffer (not first-time))
	  (setq first-time nil)
	  (funcall tags-apropos-function regexp))))
    (etags-tags-apropos-additional regexp))
  (with-current-buffer "*Tags List*"
    (eval-and-compile (require 'apropos))
    (apropos-mode)
    ;; apropos-mode is derived from fundamental-mode and it kills
    ;; all local variables.
    (setq buffer-read-only t)))

(defun my-read-tags-one (tag-file others)
  (when tag-file
    (visit-tags-table tag-file)
    (my-read-tags-one (car others) (cdr others))))

(defun my-read-tags (dir)
  "Reads all TAGS# files in a directory."
  (interactive "DDirectory: ")
  (let ((all-tags-files (directory-files dir t "TAGS[0-9]+")))
    (my-read-tags-one (car all-tags-files) (cdr all-tags-files))))
