;;; .emacs --- my custom .emacs file
;;; Commentary:
;;; Code:


;; Repositories
(setq-default package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
				 ("melpa" . "https://melpa.org/packages/")
				 ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
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
(set-face-attribute 'default nil :height 160)

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
  :functions doom-modeline-mode
  :init (doom-modeline-mode t)
  :custom (doom-modeline-height 30))

;; Theme
(use-package doom-themes :defer t)
;; (load-theme 'doom-challenger-deep t)
;; (load-theme 'doom-dark+ t)
;; (load-theme 'doom-dracula t)
;; (load-theme 'doom-palenight
(load-theme 'doom-tomorrow-night t)

;; Vertical lines
(use-package highlight-indentation)

;; Better parem mode (after theme)
(set-face-attribute 'show-paren-match nil :background "yellow" :foreground "black")


;; ===================================================================
;; ========================= Custom packages =========================
;; ===================================================================

;; Asdf
(add-to-list 'exec-path "~/.asdf/shims/")
(add-to-list 'exec-path "/opt/homebrew/opt/asdf/libexec/bin/")

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
  :bind
  ("C-x p" . treemacs-add-and-display-current-project-exclusively))

;; Dired icons
(use-package treemacs-icons-dired
  :ensure t
  :functions treemacs-icons-dired-mode
  :config
  (treemacs-icons-dired-mode))

;; Lsp mode
;; Install the engines via npm
(use-package lsp-mode
  :init
  (setq-default lsp-keymap-prefix "C-c p")
  :hook (((python-mode
	   html-mode
	   css-mode
	   js-mode
	   c++-mode) . lsp)
         (lsp-modr . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-file-watch-threshold 10000))

;; Lsp UI
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind (("s-i" . lsp-ui-doc-glance))
  :custom
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-delay 0)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-sideline-enable nil))

;; Lsp treemacs
(use-package lsp-treemacs
  :after lsp)

;; Company autocomplete
(use-package company
  :ensure t
  :functions global-company-mode
  :config
  (setq-default company-idle-delay 0
		company-minimum-prefix-length 1
		company-show-quick-access t
		company-tooltip-limit 10
		company-tooltip-align-annotations t)
  (global-company-mode t))

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

;; Flycheck
(use-package flycheck
  :ensure t
  :functions global-flycheck-mode
  :init (global-flycheck-mode))

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

;; Major mode CSV
(use-package csv-mode
  :ensure t
  :functions (csv-header-line)
  :hook((csv-mode . csv-header-line)
	(csv-header-line . csv-align-mode)))

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

;; Games
(use-package 2048-game)

;; ================================================================
;; ========================= Langs config =========================
;; ================================================================

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
  :init (setq-default lsp-pyright-python-executable-cmd "python3"))

;; (use-package python-mode
;;   :ensure t
;;   :custom
;;   (python-shell-interpreter "python3")
;;   :hook (python-mode . lsp-deferred))


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

;; Ignore case in completion
(setq read-buffer-completion-ignore-case t)

;; Garbage collector
(setq gc-cons-threshold (* 50 1000 1000))

;; Custom functions
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
(global-set-key (kbd "C-c d") 'duplicate-current-line) ;; duplicate-line
;; Window size
(global-set-key (kbd "M-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-s-<down>") 'shrink-window)
(global-set-key (kbd "M-s-<up>") 'enlarge-window)


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
