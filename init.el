(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)


(require 'dash)
(require 's)
(require 'f)
(require 'use-package)

(defun load-local (file)
  (load (f-expand file user-emacs-directory)))

(load-local "defuns")
(load-local "keybindings")
(load-local "iuscheme")

;; --------------------------------------------------
;;;;;;;;;;;;;;;     Preferences     ;;;;;;;;;;;;;;;;;
;; --------------------------------------------------

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;; programming utils
(use-package company
  :config (global-company-mode))

(use-package multiple-cursors
  :defer t
  :bind (("M->" . mc/mark-next-like-this)
         ("C-." . mc/mark-next-like-this)
         ("M-<" . mc/mark-previous-like-this)
         ("C-," . mc/mark-previous-like-this)
         ("C-x ." . mc/mark-all-like-this)
         ("C-x /" . mc/edit-lines)))

(use-package key-chord
  :config
  (key-chord-mode +1))

(use-package projectile
  :init (projectile-mode)
  :config
  (progn
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'ido)
    (global-set-key (kbd "C-c C-f") 'projectile-find-file)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

(use-package crux)

(use-package magit
  :config
  (progn
    (define-key magit-file-mode-map (kbd "C-x g") nil)
    (setq magit-last-seen-setup-instructions "1.4.0")
    (key-chord-define-global "mg" 'magit-status)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package undo-tree
  :config (progn
            (define-key undo-tree-map (kbd "C-x u") nil)
            (global-undo-tree-mode)
            (key-chord-define-global "uu" 'undo-tree-visualize)))

(use-package flycheck
  :config (progn
            (define-key flycheck-mode-map (kbd "M-n") #'flycheck-next-error)
            (define-key flycheck-mode-map (kbd "M-p") #'flycheck-previous-error)))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package elisp-slime-nav
  :config
  (progn
    (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
      (add-hook hook 'elisp-slime-nav-mode))))

(use-package ace-window
  :config
  (progn
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(use-package discover-my-major
  :config
  (define-key 'help-command (kbd "C-m") 'discover-my-major))

(use-package swiper
  :config
  :bind ("C-s" . swiper))

;; IDO completion

(use-package ido
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
  :init (ido-vertical-mode 1))

(use-package flx-ido
  :init (flx-ido-mode 1))

(use-package smex)

(use-package eldoc
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook (lambda () (eldoc-mode 1)))))

;; (use-package lsp-mode
;;   :config (setq lsp-enable-snippet nil))

;; managing parens

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
  :config
  (progn
    (global-set-key (kbd "C-c C-s") c-sp-keymap)
    (smartparens-global-strict-mode -1)
    (smartparens-global-mode 1)
    (sp-pair "'" nil :actions :rem)
    (sp-pair "`" nil :actions :rem)
    (dolist (hook '(cider-repl-mode-hook
                    clojure-mode-hook
                    emacs-lisp-mode-hook
                    list-mode-hook
                    list-interaction-mode-hook
                    go-mode-hook
                    ruby-mode-hook
                    rust-mode-hook
                    haskell-mode-hook
                    js2-mode-hook
                    racket-repl-mode-hook
                    racket-mode-hook
                    agda2-mode-hook))
      (add-hook hook (lambda () (smartparens-strict-mode 1))))))

(use-package cider
  :config
  (progn
    (add-hook 'cider-mode-hook 'eldoc-mode)
    (add-hook 'cider-repl-mode-hook #'eldoc-mode)
    (setq nrepl-hide-special-buffers t)
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (setq cider-popup-stacktraces nil)
    (setq cider-repl-display-help-banner nil)
    (define-key cider-repl-mode-map (kbd "M-p") 'cider-repl-backward-input)
    (define-key cider-repl-mode-map (kbd "M-n") 'cider-repl-forward-input))
  :bind (("C-c r". cider-repl-reset)))

;; major modes

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljx\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode))
  :config (progn
            ;; korma macros
            (put-clojure-indent 'select 1)
            (put-clojure-indent 'insert 1)))

(use-package go-mode
  :config
  (progn
    (defun csk-go-mode-hooks ()
      (add-to-list 'exec-path "~/chai/go/bin")
      (setq gofmt-command "goimports")
      (setq tab-width 8 indent-tabs-mode 1))

    (add-hook 'go-mode-hook 'csk-go-mode-hooks)))

(use-package ruby-mode
  :init
  (progn
    (add-hook 'ruby-mode-hook '(lambda () (define-key ruby-mode-map "\C-m" 'newline-and-indent)))))

(use-package feature-mode
  :mode (("\.feature$" . feature-mode)))

(use-package rust-mode
  :hook ((rust-mode . lsp)
         (rust-mode . flycheck-mode))
  :config (progn (setq lsp-prefer-flymake nil)))

(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode)))

(use-package markdown-mode
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)))

(use-package yaml-mode
  :defer t
  :mode ("\\.yml$" . yaml-mode))

(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)))

(use-package racket-mode
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
            (put 'letpacked 'racket-indent-function 1)
            (put 'letew 'racket-indent-function 1)
            (put 'letscalar 'racket-indent-function 1)
            (put 'if0 'racket-indent-function 1)))

(use-package cask-mode)

(use-package idris-mode
  :init
  (add-hook 'idris-mode-hook
            (lambda ()
              (remove-hook 'before-save-hook 'whitespace-cleanup)
              (remove-hook 'before-save-hook 'clean-up-buffer-or-region))))

(use-package proof-site
  :config (progn
            (setq coq-compile-before-require 't)
            (set-face-attribute 'proof-locked-face nil
                                :background "#282622")))

(use-package dockerfile-mode)

(defun enable-dante ()
  (or (string= (projectile-project-root) "/home/ckoparkar/chai/tree-velocity/")
      (string= (projectile-project-root) "/home/ckoparkar/chai/fusion-plugin/")
      ;; (string= (projectile-project-root) "/home/ckoparkar/chai/classes/distributed-systems/")
      ))

(defun ghc-version ()
  (cond
   ((string= (projectile-project-root) "/home/ckoparkar/chai/tree-velocity/") "ghc-8.4.3")
   ((string= (projectile-project-root) "/home/ckoparkar/chai/fusion-plugin/") "ghc-8.8.2")
   (t "ghc-8.6.5")))

(use-package dante
  :init
  (progn
    (setq flycheck-error-list-minimum-level 'warning)
    (add-hook 'dante-mode-hook 'flycheck-mode)
    (setq dante-repl-command-line `("cabal" "new-repl" "--builddir=dist/dante" "--with-ghc" (ghc-version)))
    (add-hook 'haskell-mode-hook '(lambda () (when (enable-dante) (dante-mode))))))

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
  :config
  (progn
    (add-hook 'haskell-mode-hook 'haskell-style)
    (define-key haskell-mode-map (kbd "C-c C-s") nil)
    (setq haskell-ask-also-kill-buffers nil)))

;; Misc stuff

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

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

;; Remap left-command key to alt in mac
(setq ns-command-modifier 'meta)

;; Set auto-scroll off in shell mode
(remove-hook 'comint-output-filter-functions
             'comint-postoutput-scroll-to-bottom)

;; Hide minor modes emacs
(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; Responsible white space
(responsible-whitespace)

;; Theme
(load-theme 'default-black t)

(set-face-attribute 'default nil :font "Monaco-12")

(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize))

(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

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
                                        rust-mode conf-toml-mode))

(setq auto-whitespace-free-modes '(latex-mode plain-tex-mode makefile-gmake-mode))

;; Safe local variables
(setq safe-local-variable-values '((checkdoc-package-keywords-flag)
                                   (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")
                                   (buffer-file-coding-system . utf-8-unix)))

;; Truly misc.
(set-default 'indicate-empty-lines t)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t inhibit-startup-echo-area-message t)
(setq column-number-mode 1)
(put 'narrow-to-region 'disabled nil)
(setq indent-tabs-mode nil)
(blink-cursor-mode 0)


;; --------------------------------------------------
;;;;;;;;;;;;;;;     C/C++     ;;;;;;;;;;;;;;;;;;;;
;; --------------------------------------------------

(use-package cc-mode
  :defer t
  :config
  (progn
    ;; C/C++ mode settings
    (setq-default c-basic-offset 4 c-default-style "linux")
    ;; (setq-default tab-width 4 indent-tabs-mode t)
    (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
    (setq mf--source-file-extension "cpp")
    (add-hook 'c++-mode-hook (lambda () (smartparens-mode 1)))
    (add-hook 'c-mode-hook (lambda ()
                             (setq comment-start "//"
                                   comment-end  "")
                             (smartparens-mode 1)))))

;; --------------------------------------------------
;;;;;;;;;;;;;;;       Agda       ;;;;;;;;;;;;;;;;;;;;
;; --------------------------------------------------

;; (load-file "/home/ckoparkar/.cabal/share/x86_64-linux-ghc-8.2.2/Agda-2.5.3/emacs-mode/agda2.el")

;; (defvar c-agda-unicode
;;   '(("\\bn" "ℕ")
;;     ("\\bb" "𝔹")
;;     ("\\bl" "𝕃")
;;     ("\\bs" "S")
;;     ("\\bt" "T")
;;     ("\\bv" "𝕍")
;;     ("\\cv" "O")
;;     ("\\comp" " ")
;;     ("\\m" "ÞÑ")
;;     ("\\om" "ω")))

;; (use-package agda-mode)

;; (custom-set-faces
;;  '(agda2-highlight-coinductive-constructor-face ((t (:foreground "#aaffcc"))))
;;  '(agda2-highlight-datatype-face ((t (:foreground "light blue"))))
;;  '(agda2-highlight-field-face ((t (:foreground "#ff99cc"))))
;;  '(agda2-highlight-function-face ((t (:foreground "#66ccff"))))
;;  '(agda2-highlight-inductive-constructor-face ((t (:foreground "#ccffaa"))))
;;  '(agda2-highlight-keyword-face ((t (:foreground "#ffaa00"))))
;;  '(agda2-highlight-module-face ((t (:foreground "#ffaaff"))))
;;  '(agda2-highlight-number-face ((t (:foreground "light green"))))
;;  '(agda2-highlight-postulate-face ((t (:foreground "#ff7766"))))
;;  '(agda2-highlight-primitive-face ((t (:foreground "#66ccff"))))
;;  '(agda2-highlight-primitive-type-face ((t (:foreground "light blue"))))
;;  '(agda2-highlight-record-face ((t (:foreground "light blue"))))
;;  '(agda2-highlight-string-face ((t (:foreground "#aaffff")))))

;; LLVM
(load-file "~/.emacs.d/site-lisp/llvm-mode.el")
