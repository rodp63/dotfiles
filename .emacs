;;; .emacs --- my custom .emacs file
;;; Commentary:
;;; Code:


;; Package repositories
(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

;; Use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ==============================================================
;; ========================= Appearance =========================
;; ==============================================================

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(98 . 98))
(add-to-list 'default-frame-alist '(alpha . (98 . 98)))

;; Disable visible scrollbar
(scroll-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable tooltip
(tooltip-mode -1)

;; Fringe size
(set-fringe-mode 10)

;; Encoding
(set-default-coding-systems 'utf-8)

;; Font
(set-face-attribute 'default nil :height 150)

;; Line spacing
(setq-default line-spacing 2)

;; ;; Maximize
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Preserve cursor when scrolling
;; (setq scroll-preserve-screen-position 'always)

;; Don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

;; Doom
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t)
  :custom (doom-modeline-height 30))

;; Theme
(use-package doom-themes :defer t)
;; (load-theme 'doom-challenger-deep t)
;; (load-theme 'doom-dark+ t)
;; (load-theme 'doom-dracula t)
;; (load-theme 'doom-palenight t)
(load-theme 'doom-tomorrow-night t)

;; Vertical lines
(use-package indent-bars
  :ensure t
  :hook (prog-mode . indent-bars-mode))
  ;; :custom
  ;; (indent-bars-no-descend-lists 'skip)
  ;; (indent-bars-treesit-support t)
  ;; (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; (indent-bars-color '(highlight :face-bg t :blend 0.5)))

;; Better parem mode (after theme)
(set-face-attribute 'show-paren-match nil :background "yellow" :foreground "black")

;; Nyan bar
(use-package nyan-mode
  :functions nyan-mode
  :config
  (setq-default nyan-animate-nyancat t
		nyan-wavy-trail t
		nyan-bar-length 24)
  (nyan-mode t))

;; ===================================================================
;; ========================= Custom packages =========================
;; ===================================================================

;; Ag
(use-package ag
  :config
  (setq-default ag-highlight-search t)
  (setq-default ag-reuse-buffers 't))

;; Setup evil
(use-package evil
  :functions evil-mode
  :config
  (evil-mode t)
  (setq-default evil-default-state 'emacs))

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :functions (treemacs-follow-mode)
  :bind
  ("C-x p" . treemacs-add-and-display-current-project-exclusively)
  :config
  (treemacs-follow-mode -1))

;; Dired icons
(use-package treemacs-icons-dired
  :ensure t
  :functions treemacs-icons-dired-mode
  :config
  (treemacs-icons-dired-mode)
  (setq-default treemacs-width 30))

;; Ignore case in completion
(setq completion-ignore-case t)

;; Company autocomplete
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1)
  (company-show-quick-access t)
  (company-tooltip-limit 10)
  (company-tooltip-align-annotations t))

;; Font packages
(use-package nerd-icons) ;; M-x nerd-icons-install-fonts
(use-package all-the-icons) ;; M-x all-the-icons-install-fonts

;; All the icons modes
(use-package all-the-icons-completion
  :ensure t
  :functions all-the-icons-completion-mode
  :init(all-the-icons-completion-mode))

(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

;; Swiper
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

;; Drag stuff
(use-package drag-stuff
  :functions (drag-stuff-global-mode
	      drag-stuff-define-keys)
  :init
  (drag-stuff-global-mode t)
  :config
  (drag-stuff-define-keys))

;; Colorful delimiters (for elisp only)
(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;; Which key
(use-package which-key
  :functions which-key-mode
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq-default which-key-idle-delay 1))

;; Help
(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ("C-h z" . helpful-at-point))

;; Doc helpers
(use-package eldoc-box
  :ensure t
  :custom-face
  (eldoc-box-body
   ((t (:family "Helvetica Neue"
        :height 1.1
        :background "#282c34"
        :foreground "#bbc2cf"))))
  (eldoc-box-border
   ((t (:background "#61afef")))))
;; :hook (eglot-managed-mode . eldoc-box-hover-mode))

;; Major mode CSV
(use-package csv-mode
  :ensure t
  :functions (csv-header-line)
  :hook((csv-mode . csv-header-line)
	(csv-mode . csv-align-mode)))

;; Major mode ipython
(use-package ein
  :ensure t)

;; Major mode YAML
(use-package yaml-mode
  :ensure t)

;; Major mode Dockerfile
(use-package dockerfile-mode
  :ensure t)

;; Major mode for git files
(use-package git-modes
  :ensure t)

;; Major mode for crontab
(use-package crontab-mode
  :ensure t)

;; Major mode for pip-requiriments
(use-package pip-requirements
  :ensure t)

;; Major mode for dotenv files
(use-package dotenv-mode
  :ensure t)

;; Major mode for JS
(use-package typescript-mode
  :ensure t)

;; Major mode for Hashicorp (packer)
(use-package hcl-mode
  :ensure t)

;; Major mode for Terraform
(use-package terraform-mode
  :ensure t)

;; Major mode for systemd units
(use-package systemd
  :ensure t)

;; Major go mode
(use-package go-mode
  :ensure t)

;; Markdown mode
(use-package markdown-mode
  :ensure t)

;; Jinja 2 mode
(use-package jinja2-mode
  :ensure t)

;; Games
(use-package 2048-game)

;; ================================================================
;; ========================= Langs config =========================
;; ================================================================

;; Tree-sitter: grammar sources
;; Install them with: M-x treesit-install-language-grammar <lang>
(dolist (grammar
         '((python "https://github.com/tree-sitter/tree-sitter-python")
           (go "https://github.com/tree-sitter/tree-sitter-go")))
  (add-to-list 'treesit-language-source-alist grammar))

;; Prefer Tree-sitter modes over their traditional counterparts
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

;; eglot
(use-package eglot
  :ensure nil
  :hook ((python-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0))

;; pyvenv
(use-package pyvenv
  :ensure t
  :hook (python-ts-mode . pyvenv-mode)
  :config
  (setq-default pyvenv-mode-line-indicator
		'(pyvenv-virtual-env-name (" [venv] "))))

;; HTML indentation
(setq-default sgml-basic-offset 4)

;; C indentation
(setq-default c-basic-offset 4)


;; =================================================================
;; ========================= Customization =========================
;; =================================================================

;; Linum modes
(column-number-mode)
(global-hl-line-mode t)

;; Custom hooks
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda ()
		   (display-line-numbers-mode 1)
		   (electric-pair-mode 1))))

;; Load files in the same window
(setq ns-pop-up-frames nil)

;; Disable startup message
(setq inhibit-startup-message t)

;; No tmp files
(setq create-lockfiles nil)
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;; Garbage collector
(setq gc-cons-threshold (* 100 1024 1024))

;; Amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024))

;; Disable clipboard
(setq select-enable-clipboard t)
(setq select-enable-primary nil)

;; Custom functions
(defun delete-backward-word ()
  "Delete backward word without yanking."
  (interactive)
  (push-mark)
  (backward-word)
  (delete-region (point) (mark)))

(defun select-current-word ()
  "Select current word."
  (interactive)
  (backward-word)
  (set-mark (point))
  (forward-word))

(defun select-current-line ()
  "Select current line."
  (interactive)
  (move-beginning-of-line 1)
  (set-mark (point))
  (move-end-of-line 1))

(defun duplicate-current-line ()
  "Duplicate the current line."
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (insert (buffer-substring (point) (line-end-position)))
    (newline)))

(defun close-all-buffers ()
  "Close all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; Keybindings

;; General
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; escape
(global-set-key (kbd "s-r") 'revert-buffer) ;; revert-buffer
(global-set-key (kbd "C-c w") 'select-current-word) ;; select-word
(global-set-key (kbd "C-c l") 'select-current-line) ;; select-line
(global-set-key (kbd "C-c u") 'duplicate-current-line) ;; duplicate-line
(global-set-key (kbd "C-c d") 'kill-whole-line) ;; kill-line
(global-set-key (kbd "M-DEL") 'delete-backward-word) ;; delete-backward-word
;; Swap forward and backward functions
(global-set-key (kbd "C-f") 'forward-word)
(global-set-key (kbd "M-f") 'forward-char)
(global-set-key (kbd "C-b") 'backward-word)
(global-set-key (kbd "M-b") 'backward-char)
;; Window size
(global-set-key (kbd "M-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-s-<down>") 'shrink-window)
(global-set-key (kbd "M-s-<up>") 'enlarge-window)
;; Navigation
(global-set-key (kbd "s-i") #'eldoc-box-help-at-point)

;; Ensure last execution
(defvar hl-line-face)
(set-face-background hl-line-face "black")

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s seconds with %d garbage collections."
		     (emacs-init-time "%.2f")
		     gcs-done)))

;;; .emacs ends here
