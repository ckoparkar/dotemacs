;; defuns.el
;;
;; Exports custom utility functions

(require 'cl)

(defun duplicate-line ()
  "Clone line at cursor, leaving the latter intact."
  (interactive)
  (let ((kill-read-only-ok t) deactivate-mark)
    (toggle-read-only 1)
    (kill-whole-line)
    (toggle-read-only 0)
    (yank)
    ;; otherwise this line "kills" ;-), the first entry in the ring
    (setq kill-ring (reverse (cdr kill-ring)))
    (backward-char)))

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command. If no region is selected and current line is not blank and we are not at the end of the line, then comment current line. Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun google (arg)
  "Googles a query or region if any.
With prefix argument, wrap search query in quotes."
  (interactive "P")
  (let ((query
         (if (region-active-p)
             (buffer-substring (region-beginning) (region-end))
           (read-string "Google: "))))
    (when arg (setq query (concat "\"" query "\"")))
    (browse-url
     (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" query))))

(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " ac")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . "")
    (highlight-parentheses-mode . "")
    (magit-auto-revert-mode . "")
    (guide-key-mode . "")
    (smartparens-mode . "")
    (projectile-mode . "")
    (cider-mode . "")

    ;; Major modes
    (lisp-interaction-mode . "λ")
    (clojure-mode . "λ")
    (haskell-mode . ">>=")
    (python-mode . "Py")
    (emacs-lisp-mode . "λ"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(defun cider-repl-reset ()
  (interactive)
  (set-buffer
   (car (-filter (lambda (buf-name) (s-starts-with? "*cider-repl" buf-name))
                 (-map (lambda (buf) (buffer-name buf)) (buffer-list)))))
  (goto-char (point-max))
  (insert "(reset)")
  (cider-repl-return))

;; NOTE: (region-beginning) and (region-end) are not saved in
;; variables since they can change after each clean step.
(defun clean-up-buffer-or-region ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (if (member major-mode auto-indent-free-modes)
        (deactivate-mark)
      (progn (untabify (region-beginning) (region-end))
             (indent-region (region-beginning) (region-end))))
    (if (member major-mode auto-whitespace-free-modes)
        (deactivate-mark)
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (delete-trailing-whitespace)))))

(defun c-whitespace-cleanup ()
  "Wrapper around `whitespace-cleanup' that respects `auto-whitespace-free-modes'"
  (unless (member major-mode auto-whitespace-free-modes)
    (whitespace-cleanup)))

(defun smartparens-dedent-all ()
  "Dedent untill all ) are properly dedented.
Invoke from line containing trailing parens."
  (interactive)
  (while (equal (string (char-after)) ")")
    (sp-dedent-adjust-sexp))
  (kill-whole-line))

(defun 4clojure-check ()
  "Check the answer and show the next question if it worked."
  (interactive)
  (unless
      (save-excursion
        ;; Find last sexp (the answer).
        (goto-char (point-max))
        (forward-sexp -1)
        ;; Check the answer.
        (cl-letf ((answer
                   (buffer-substring (point) (point-max)))
                  ;; Preserve buffer contents, in case you failed.
                  ((buffer-string)))
          (goto-char (point-min))
          (while (search-forward "__" nil t)
            (replace-match answer))
          (string-match "failed." (4clojure-check-answers))))))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY.
Source: https://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00087.html"
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun racket-scratch ()
  "Create/switch to a scratch buffer for Racket."
  (interactive)
  (let ((buf (get-buffer-create "*racket-scratch*")))
    (switch-to-buffer buf)
    (racket-mode)
    (save-excursion
      (goto-char (point-min))
      (unless (looking-at-p "#")
        (insert "#lang racket\n\n")))))

(defun responsible-whitespace ()
  (interactive)
  (mapc (lambda (x)
          (add-hook 'before-save-hook x))
        '(clean-up-buffer-or-region c-whitespace-cleanup)))

(defun unresponsible-whitespace ()
  (interactive)
  (mapc (lambda (x)
          (remove-hook 'before-save-hook x))
        '(clean-up-buffer-or-region c-whitespace-cleanup)))

(defun disable-arrow-keys ()
  "Disable arrow-keys"
  (interactive)
  (global-unset-key (kbd "<up>"))
  (global-unset-key (kbd "<down>"))
  (global-unset-key (kbd "<left>"))
  (global-unset-key (kbd "<right>")))

(defun enable-arrow-keys ()
  "Enable arrow keys."
  (interactive)
  (global-set-key (kbd "<up>") 'previous-line)
  (global-set-key (kbd "<down>") 'next-line)
  (global-set-key (kbd "<right>") 'right-char)
  (global-set-key (kbd "<left>") 'left-char))

(defun edit-init-el ()
  "Open my init.el file."
  (find-file "~/.emacs.d/init.el"))

(fset 'let*->set!
      [?\C-= ?\C-x ?\C-m ?m ?c ?/ ?m ?a ?r ?k ?- ?a ?l ?l ?\C-n return ?\[ return ?\C-g ?s ?e ?t ?! ?  ?\C-b ?\C-b ?\C-b ?\C-b ?\C-b ?\C-b ?\C-c ?\C-p ?\C-c ?\C-p ?\C-g ?\C-c ?\C-s ?s ?\C-w ?b ?e ?g ?i ?n ?  ?\C-x ?\C-s])

(defun haskell-insert-language-pragma-at-point ()
  (interactive)
  (insert "{-# LANGUAGE  #-}")
  (backward-char 4))

(provide 'defuns)
;;; defuns.el ends here
