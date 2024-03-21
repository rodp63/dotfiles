;;; .emacs --- my custom .emacs file
;;; Commentary:
;;; Code:

;; =======================================================================
;; ========================= Basic customization =========================
;; =======================================================================

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(97 . 97))
(add-to-list 'default-frame-alist '(alpha . (97 . 97)))

;; Linum mode
(column-number-mode)
(global-hl-line-mode t)
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Disable startup message
(setq inhibit-startup-message t)

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
(set-face-attribute 'default nil :height 130)

;; Maximize
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))



;; ===================================================================
;; ========================= Custom packages =========================
;; ===================================================================

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

;; General keybindings
;; (use-package general
;;   :config
;;   (general-create-definer rune/leader-keys
;;     :keymaps '(normal insert visual emacs)
;;     :prefix "SPC"
;;     :global-prefix "C-SPC")
;;   (rune/leader-keys
;;     "t"  '(:ignore t :which-key "toggles")))


;; Setup evil
(use-package evil
  ;; :init
  ;; (setq evil-want-integration t)
  ;; (setq evil-want-keybinding nil)
  ;; (setq evil-want-C-u-scroll t)
  ;; (setq evil-want-C-i-jump nil)
  :functions evil-mode
  :config
  (evil-mode t)
  (setq-default evil-default-state 'emacs))
;; (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))
;; (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
;; ;; Use visual line motions even outside of visual-line-mode buffers
;; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
;; (evil-set-initial-state 'messages-buffer-mode 'normal)
;; (evil-set-initial-state 'dashboard-mode 'normal))

;; Asdf
(add-to-list 'exec-path "~/.asdf/shims/")
(add-to-list 'exec-path "/opt/homebrew/opt/asdf/libexec/bin/")

;; Lsp mode
(use-package lsp-mode
  :init
  (setq-default lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-headerline-breadcrumb-enable-diagnostics nil))

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
		company-minimum-prefix-length 2
		company-show-quick-access t
		company-tooltip-limit 10
		company-tooltip-align-annotations t)
  (global-company-mode t))

;; Font packages
(use-package nerd-icons)
(use-package all-the-icons)

;; Swiper
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

;; Doom mode line
(use-package doom-modeline
  :ensure t
  :functions doom-modeline-mode
  :init (doom-modeline-mode t)
  :custom ((doom-modeline-height 30)))

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

;; Theme
(use-package doom-themes :defer t)
;; (load-theme 'doom-challenger-deep t)
;; (load-theme 'doom-dark+ t)
;; (load-theme 'doom-dracula t)
;; (load-theme 'doom-palenight
(load-theme 'doom-tomorrow-night t)

;; Colorful delimiters
(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode)) ;; activate for elisp

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


;; =================================================================
;; ========================= Python config =========================
;; =================================================================

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
  :init (setq-default lsp-pyright-python-executable-cmd "python3"))

(use-package python-mode
  :ensure t
  :custom
  (python-shell-interpreter "python3")
  :hook (python-mode . lsp-deferred))



;; ====================================================================
;; ========================= Custom functions =========================
;; ====================================================================

;; No tmp files
(setq create-lockfiles nil)
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;; Don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

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

;; Keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; escape
(global-set-key (kbd "s-r") 'revert-buffer) ;; revert-buffer
(global-set-key (kbd "C-c w") 'select-current-word) ;; select-word
(global-set-key (kbd "C-c l") 'select-current-line) ;; select-line
(global-set-key (kbd "C-c d") 'duplicate-current-line) ;; duplicate-line

;; Better parem mode
(set-face-attribute 'show-paren-match nil :background "yellow" :foreground "black")

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s seconds with %d garbage collections."
		     (emacs-init-time "%.2f")
		     gcs-done)))

;;; .emacs ends here
