(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(require 'cl)

(setq package-selected-packages '(fullframe fennel-mode yaml-mode use-package undo-tree swiper sml-mode smex smartparens rust-mode restclient racket-mode projectile multiple-cursors markdown-mode magit lua-mode key-chord json-mode idris-mode ido-vertical-mode highlight-parentheses go-mode flx-ido feature-mode expand-region exec-path-from-shell elisp-slime-nav dockerfile-mode discover-my-major dante crux cider cask-mode ace-window gruvbox-theme))

;; --------------------------------------------------
;;;;;;;;;;;;;;;;;;     MW stuff    ;;;;;;;;;;;;;;;;;;
;; --------------------------------------------------

(load "~/chai/mwdotfiles/mw.el")

;; --------------------------------------------------
;;;;;;;;;;;;;;;;;     Packages     ;;;;;;;;;;;;;;;;;;
;; --------------------------------------------------

(defun load-local (file)
  (load (concat user-emacs-directory "site-lisp/" file)))

(load-local "iuscheme")
(load-local "llvm-mode")
(load-local "google-c-style")

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package multiple-cursors
  :ensure t
  :bind (("M->" . mc/mark-next-like-this)
         ("C-." . mc/mark-next-like-this)
         ("M-<" . mc/mark-previous-like-this)
         ("C-," . mc/mark-previous-like-this)
         ("C-x ." . mc/mark-all-like-this)
         ("C-x /" . mc/edit-lines)))

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode +1))

(use-package projectile
  :ensure t
  :init (projectile-mode)
  :config
  (progn
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'ido)
    (global-set-key (kbd "C-c C-f") 'projectile-find-file)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

(use-package crux
  :ensure t)

(use-package magit
  :ensure t
  :config
  (progn
    (define-key magit-mode-map (kbd "C-x g") nil)
    (setq magit-last-seen-setup-instructions "1.4.0")
    (key-chord-define-global "mg" 'magit-status)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package undo-tree
  :ensure t
  :config (progn
            (define-key undo-tree-map (kbd "C-x u") nil)
            (global-undo-tree-mode)
            (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
            (setq undo-tree-auto-save-history nil)
            (key-chord-define-global "uu" 'undo-tree-visualize)))

(use-package flycheck
  :ensure t
  :config (progn
            (define-key flycheck-mode-map (kbd "M-n") #'flycheck-next-error)
            (define-key flycheck-mode-map (kbd "M-p") #'flycheck-previous-error)))

(use-package elisp-slime-nav
  :ensure t
  :config
  :hook ((emacs-lisp-mode-hook . elisp-slime-nav-mode)
         (ielm-mode-hook . elisp-slime-nav-mode)))

(use-package ace-window
  :ensure t
  :config
  (progn
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(use-package discover-my-major
  :ensure t
  :config
  (define-key 'help-command (kbd "C-m") 'discover-my-major))

(use-package swiper
  :ensure t
  :config
  :bind ("C-s" . swiper))

(use-package ido
  :ensure t
  :init
  (progn
    (ido-mode 1)
    (ido-everywhere t))
  :config
  (progn
    (defun ido-my-keys ()
      (define-key ido-common-completion-map (kbd "C-n") 'ido-next-match)
      (define-key ido-common-completion-map (kbd "C-p") 'ido-prev-match))
    (setq ido-auto-merge-work-directories-length nil)
    (add-hook 'ido-setup-hook 'ido-my-keys)
    (setq ido-ubiquitous-mode t)
    (setq ido-use-filename-at-point 'guess)
    (setq ido-create-new-buffer 'always)))

(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode 1))

(use-package flx-ido
  :ensure t
  :init (flx-ido-mode 1))

(use-package smex
  :ensure t
  :config (smex-initialize))

(use-package eldoc
  :ensure t
  :hook ((emacs-lisp-mode-hook . eldoc-mode)))

(use-package fullframe
  :ensure t
  :config (progn
            (fullframe magit-status magit-mode-quit-window)))

(use-package fill-column-indicator
  :ensure t
  :config (progn (setq fci-rule-color "black")
                 (setq fci-rule-column 80)
                 (add-hook 'c-mode-hook (lambda () (fci-mode)))
                 (add-hook 'c++-mode-hook (lambda () (fci-mode)))
                 (add-hook 'rust-mode-hook (lambda () (fci-mode)))
                 (add-hook 'haskell-mode-hook (lambda () (fci-mode)))))

;; managing parens

(use-package highlight-parentheses
  :ensure t
  :config (global-highlight-parentheses-mode t))

(defvar c-sp-keymap
  (let ((map (make-sparse-keymap))
        (forward-map (make-sparse-keymap))
        (backward-map (make-sparse-keymap))
        (transpose-map (make-sparse-keymap)))
    (define-key map (kbd "r") #'sp-raise-sexp)
    (define-key map (kbd "s") #'sp-splice-sexp)
    ;; a keymap to hold forward* keybindings
    (define-key map (kbd "f") forward-map)
    (define-key forward-map (kbd "s") #'sp-forward-slurp-sexp)
    (define-key forward-map (kbd "b") #'sp-forward-barf-sexp)
    ;; a keymap to hold backward* keybindings
    (define-key map (kbd "b") backward-map)
    (define-key backward-map (kbd "s") #'sp-backward-slurp-sexp)
    (define-key backward-map (kbd "b") #'sp-backward-barf-sexp)
    map))

(use-package smartparens
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-c C-s") c-sp-keymap)
    (smartparens-global-strict-mode -1)
    (smartparens-global-mode 1)
    (sp-pair "'" nil :actions :rem)
    (sp-pair "`" nil :actions :rem)
    (dolist (hook '(clojure-mode-hook
                    emacs-lisp-mode-hook
                    lisp-mode-hook
                    lisp-interaction-mode-hook
                    go-mode-hook
                    ruby-mode-hook
                    rust-mode-hook
                    haskell-mode-hook
                    js2-mode-hook
                    racket-repl-mode-hook
                    racket-mode-hook
                    agda2-mode-hook
                    fennel-mode
                    rust-mode-hook
                    c-mode-hook))
      (add-hook hook (lambda () (smartparens-strict-mode 1))))))

;; major modes

(use-package clojure-mode
  :ensure t
  :defer t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljx\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode))
  :config (progn
            ;; korma macros
            (put-clojure-indent 'select 1)
            (put-clojure-indent 'insert 1)))

(use-package fennel-mode
  :ensure t)

(use-package go-mode
  :ensure t
  :defer t
  :config
  (progn
    (defun csk-go-mode-hooks ()
      (add-to-list 'exec-path "~/chai/go/bin")
      (setq gofmt-command "goimports")
      (setq tab-width 8 indent-tabs-mode 1))

    (add-hook 'go-mode-hook 'csk-go-mode-hooks)))

(use-package ruby-mode
  :ensure t
  :defer t)

(use-package feature-mode
  :ensure t
  :defer t
  :mode (("\.feature$" . feature-mode)))

(use-package rust-mode
  :ensure t
  :defer t
  :config (progn (setq rust-format-on-save t)))

(use-package lua-mode
  :ensure t
  :defer t
  :mode (("\\.lua\\'" . lua-mode)))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode ("\\.yml$" . yaml-mode))

(use-package restclient
  :ensure t
  :defer t
  :mode (("\\.rest\\'" . restclient-mode)))

(use-package racket-mode
  :ensure t
  :defer t
  :mode (("\\.rkt\\'" . racket-mode) ("\\.gib\\'" . racket-mode) ("\\.pie\\'" . racket-mode))
  :config (progn
            (define-key racket-repl-mode-map "\r" 'scheme-return)
            (put 'fresh 'racket-indent-function 1)
            (put 'run 'racket-indent-function 2)
            (put 'run* 'racket-indent-function 1)
            (put 'conde 'racket-indent-function 0)
            (put 'union-case 'racket-indent-function 2)
            (put 'pmatch 'racket-indent-function 1)
            (put 'go-on 'racket-indent-function 1)
            (put 'letregion 'racket-indent-function 1)
            (put 'letloc 'racket-indent-function 1)
            (put 'fn 'racket-indent-function 2)
            (put 'term 'racket-indent-function 0)
            (put 'letpacked 'racket-indent-function 1)
            (put 'letew 'racket-indent-function 1)
            (put 'letscalar 'racket-indent-function 1)
            (put 'if0 'racket-indent-function 1)))

(use-package cask-mode
  :ensure t)

(use-package idris-mode
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t)

(use-package proof-general
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t)

(defun haskell-style ()
  "Sets the current buffer to use Haskell Style. Meant to be
  added to `haskell-mode-hook'"
  (interactive)
  (setq tab-width 4
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-starter-offset 4
        haskell-indentation-where-pre-offset 2
        haskell-indentation-where-post-offset 2))

(use-package haskell-mode
  :ensure t
  :defer t
  :hook ((haskell-mode-hook . haskell-style))
  :config
  (progn
    (define-key haskell-mode-map (kbd "C-c C-s") nil)
    (setq haskell-ask-also-kill-buffers nil)))

(use-package sml-mode
  :load-path "~/.emacs.d/site-lisp"
  :defer t
  :mode (("\\.fun\\'" . sml-mode)
         ("\\.sig\\'" . sml-mode))
  :config (progn
            (setq sml-indent-level 2)
            (setq sml-indent-args 2)
            (setq indent-tabs-mode nil)))

(use-package cc-mode
  :ensure t
  :defer t
  :config
  (progn
    (setq-default c-basic-offset 4 c-default-style "linux")
    (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
    (add-hook 'c-mode-hook (lambda ()
                             (setq comment-start "//"
                                   comment-end   "")
                             (google-set-c-style)
                             (put 'c-electric-paren 'delete-selection nil)
                             (put 'c-electric-brace 'delete-selection nil)))))

(use-package lsp-mode
  :config (progn
            (lsp-headerline-breadcrumb-mode -1)
            (lsp-modeline-diagnostics-mode -1)))

(dolist (hook '(tex-mode-hook latex-mode-hook plain-tex-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(use-package nix-mode
  :ensure t
  :defer t)


;; --------------------------------------------------
;;;;;;;;;;;;;;;;;;     Defuns     ;;;;;;;;;;;;;;;;;;;
;; --------------------------------------------------

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

;; Alist for `clean-mode-line'.
;;
;; When you add a new element to the alist, keep in mind that you
;; must pass the correct minor/major mode symbol and a string you
;; want to use in the modeline *in lieu of* the original.
(setq mode-line-cleaner-alist
      `((auto-complete-mode . " ac")
        (eldoc-mode . "")
        (abbrev-mode . "")
        (undo-tree-mode . "")
        (highlight-parentheses-mode . "")
        (magit-auto-revert-mode . "")
        (smartparens-mode . "")
        (projectile-mode . "")
        (cider-mode . "")
        (company-mode . "")
        (elisp-slime-nav-mode . "")
        (hs-minor-mode . "")
        (lisp-interaction-mode . "λ")
        (clojure-mode . "λ")
        (haskell-mode . ">>=")
        (python-mode . "Py")
        (emacs-lisp-mode . "λ")))

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
      (progn
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (delete-trailing-whitespace))
        (whitespace-cleanup)))))

(defun smartparens-dedent-all ()
  "Dedent untill all ) are properly dedented.
Invoke from line containing trailing parens."
  (interactive)
  (while (equal (string (char-after)) ")")
    (sp-dedent-adjust-sexp))
  (kill-whole-line))

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
        '(clean-up-buffer-or-region)))

(defun unresponsible-whitespace ()
  (interactive)
  (mapc (lambda (x)
          (remove-hook 'before-save-hook x))
        '(clean-up-buffer-or-region)))

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
  (interactive)
  (find-file "~/.emacs.d/init.el"))


;; --------------------------------------------------
;;;;;;;;;;;;;;;;     Keybindings     ;;;;;;;;;;;;;;;;
;; --------------------------------------------------

;; Avoid backspace
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Better help functions
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)

;; Extra navigation
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-x e") 'end-of-buffer)
(global-set-key (kbd "C-x a") 'beginning-of-buffer)
(global-set-key (kbd "<C-return>") 'crux-smart-open-line)
(global-set-key [remap move-beginning-of-line] 'prelude-move-beginning-of-line)
(global-set-key (kbd "M-p") 'ace-window)

;; Custom defuns
(global-set-key (kbd "C-x d") 'duplicate-line)
(global-set-key (kbd "C-x ;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c da") 'smartparens-dedent-all)

;; Enable smex, enhancement for M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex-major-mode-commands)

;; Bindings for Atreus
(global-set-key (kbd "C-c q") 'delete-other-windows)
(global-set-key (kbd "C-c w") 'split-window-below)
(global-set-key (kbd "C-c e") 'split-window-right)
(global-set-key (kbd "C-c r") 'delete-window)


;; --------------------------------------------------
;;;;;;;;;;;;;;;;     Misc config     ;;;;;;;;;;;;;;;;
;; --------------------------------------------------

;; Setup linum
(make-face 'linum-face)
(set-face-attribute 'linum-face nil
                    :foreground "#718c00"
                    :weight 'bold)
(setq linum-format (propertize "%4d  " 'face 'linum-face))
(global-linum-mode 1)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Delete the selection with a keypress
;; If you enable Delete Selection mode, a minor mode, then inserting text while the mark is active causes the selected text to be deleted first. This also deactivates the mark. Many graphical applications follow this convention, but Emacs does not.
(delete-selection-mode t)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Newline at end of file
;; (setq require-final-newline nil)

;; More useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Set auto-scroll off in shell mode
(remove-hook 'comint-output-filter-functions
             'comint-postoutput-scroll-to-bottom)

;; Hide minor modes emacs
(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; Responsible white space
(responsible-whitespace)

;; Theme
;; (set-face-attribute 'default nil :font "Iosevka-16" :width 'expanded)
(set-face-attribute 'default nil :font "Essential PragmataPro" :height 180)
(set-face-attribute 'region nil :background "#b3d7ff")

;; Setting up Unicode
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Autosave settings
(setq temporary-file-directory "/tmp/")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; Org mode truncates lines
(setq org-startup-truncated nil)


;; Make `clean-up-buffer-or-region` configurable.
(setq auto-indent-free-modes '(org-mode c-mode agda2-mode markdown-mode c++-mode
                                        latex-mode plain-tex-mode
                                        makefile-gmake-mode
                                        fundamental-mode
                                        haskell-cabal-mode
                                        yaml-mode python-mode rst-mode
                                        coq-mode dockerfile-mode sh-mode
                                        rust-mode conf-toml-mode sml-mode
                                        bibtex-mode idris-mode racket-mode
                                        mhtml-mode))

(setq auto-whitespace-free-modes '(latex-mode plain-tex-mode makefile-gmake-mode idris-mode))

;; Autocomplete (taken from Purcell's)

(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; Safe local variables
(setq safe-local-variable-values '((checkdoc-package-keywords-flag)
                                   (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")
                                   (buffer-file-coding-system . utf-8-unix)))

;; Some UI stuff
(global-hl-line-mode)
(save-place-mode 1)
(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))
(set-default 'indicate-empty-lines t)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t inhibit-startup-echo-area-message t)
(setq column-number-mode 1)
(put 'narrow-to-region 'disabled nil)
(setq indent-tabs-mode nil)
(blink-cursor-mode 0)

;; Important for OSX
(setq mac-command-modifier 'meta)
(setq ns-command-modifier 'meta)
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize))


;; --------------------------------------------------
;;;;;;;;;;;;;;;;   Packages to try   ;;;;;;;;;;;;;;;;
;; --------------------------------------------------

;; recentf
;; vertico
;; ibuffer
;; diminish
;; origami
