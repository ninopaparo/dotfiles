;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;; My Emacs settings

;;; Code:
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;
(setq inhibit-startup-message t)

;;
(setq user-full-name "Nino Paparo"
      user-mail-address "")

;; avoid opening UI pop up dialogs
(setq use-dialog-box nil)

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

;; Avoid creating backup files
(setq make-backup-files nil)

;; Automatically Reload Files When They Change On Disk
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; Use y/n instead of full yes/no for confirmation messages
(fset 'yes-or-no-p 'y-or-n-p)

;;
(defun check-font (my-font font-height)
  "Check if MY-FONT is installed and set to FONT-HEIGHT."
  (when (find-font (font-spec :name my-font))
   (set-face-attribute 'default nil :font my-font :height font-height)))

;;
(defun select-font (my-font font-height)
  "Select MY-FONT and FONT-HEIGHT or default to Monaco."
  (if
      (check-font my-font  font-height)
      (check-font "Monaco" 120)))

;;
(select-font "Monoid Nerd Font" 140)

;;
(if (eq window-system 'ns)
    ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Systems.html
    ;; macOs specific settings
    (setq mac-command-modifier      'meta
	  ac-option-modifier        'alt
	  mac-right-option-modifier 'alt)
  (message "keeping default M-x keybindings"))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Modeline Settings
(defun show-battery-status()
  (unless (equal "Battery status not available"
		 (battery))
    (display-battery-mode 1)))

;;
(show-battery-status)

(defun open-config-file()
  "Open init.el ."
  (interactive)(find-file "~/.emacs.d/init.el"))

;; https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; Display current time
(display-time-mode 1)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

;;
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;;
(require 'use-package)
(setq use-package-always-ensure t)
(use-package command-log-mode)

;;
(use-package swiper
  :ensure t)

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;;
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
	 :map minibuffer-local-map
	 ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height)))

;;
(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

;;
(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :ensure nil
  :commands (dired dired-jump))

;;
(use-package all-the-icons
  :if (display-graphic-p))

;;
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;;
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;;
(use-package evil-nerd-commenter)

;;
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;;
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer np/org-mode
    :prefix "SPC")

  (general-define-key
   :states '(normal visual)
   "C-x C-f" 'counsel-find-file)

  (general-define-key
   :states '(normal visual)
   :keymaps '(clojure-mode-map)
   "M-<return>" 'cider-eval-sexp-at-point)

  (general-define-key
   "M-<up>" 'move-line-up
   "M-<down>" 'move-line-down)

  (general-define-key
   :states '(normal visual)
   :keymaps '(clojure-mode-map emacs-lisp-mode-map go-mode-map rustic-mode-map)
   "M-l" 'lispyville->
   "M-h" 'lispyville-<
   "TAB" 'evil-jump-item)

  (general-create-definer np/leader-keys
    :prefix "SPC")

  (np/leader-keys
    :states 'normal
    :keymaps 'override

    "bb" '(ibuffer :which-key "ibuffer")
    "bk" '(kill-this-buffer :which-key "kill this buffer")

    "cc" 'evilnc-comment-or-uncomment-lines
    "cw" '(whitespace-mode :which-key "show whitespace")

    "h"  '(:ignore t :which-key "helper functions")
    "ho"  '(describe-symbol :which-key "describe symbol")
    "ht" '(load-theme :which-key "choose theme")

    ;; magit
    "g"  '(:ignore t :which-key "magit")
    "gs" '(magit-status :which-key "magit-status")

    ;; org-mode
    "m" '(:ignore t :which-key "org-mode")
    "ma" '(org-agenda :which-key "open org-agenda")
    "ms" '(org-occur-in-agenda-files :which-key "search in agenda")

    ;; Window Management
    "w"  '(:ignore t :which-key "window management")
    "ws" '(evil-window-split :which-key "horizontal split")
    "wv" '(evil-window-vsplit :which-key "vertical split")
    "wj" '(evil-window-down :which-key "move down")
    "wk" '(evil-window-up :which-key "move up")
    "wh" '(evil-window-left :which-key "move left")
    "wl" '(evil-window-right :which-key "move right")
    "wo" '(delete-other-windows :which-key "delete other windows")
    "ww" '(toggle-frame-fullscreen :which-key "toggle fullscreen")

    "f"  '(:ignore t :which-key "config files")
    "fp"  '(open-config-file :which-key "open config file")

    "o"  '(:ignore t :which-key "open")
    "oe" '(eshell :which-key "eshell")
    "op" '(list-packages :which-key "list-packages")
    "ot" '(treemacs :which-key "treemacs")

    "pp" '(projectile-switch-project :which-key "projectile")
    "pe" '(eval-buffer :which-key "eval buffer")

    "qq" '(evil-quit :which-key "quit emacs")

    "t" '(vterm :which-key "vterm")

    "/"   '(swiper :which-key "swiper")
    "SPC" '(dired-at-point :which-key "dired")))

;;
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t)) ; if nil, italics is universally disabled

(if (eq window-system 'ns)
    (load-theme 'doom-solarized-dark-high-contrast t)
  (load-theme 'doom-solarized-light t))

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)

;; or for treemacs users
(setq doom-themes-treemacs-theme "doom-atom")

;; use "doom-colors" for less minimal icon theme
(doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;;
(use-package vterm
  :ensure t
  :config
  (setq vterm-shell "/bin/zsh"))

;;
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode +1)
  :init
  (setq projectile-project-search-path '("~/code"))
  (setq projectile-switch-project-action #'projectile-dired))

;; Programming Modes

;; Set Type Of Line Numbering (Global Variable)
(setq display-line-numbers-type 'relative)

;;
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package lispy
  :ensure t)

(use-package lispyville
  :init
  (general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook) #'lispyville-mode)
  (general-add-hook '(clojure-mode-hook lisp-mode-hook) #'lispyville-mode)
  :config
  (lispyville-set-key-theme '(operators c-w additional)))

;; Activate line numbering in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'yaml-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook (lambda () (hs-minor-mode)))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;;
(use-package cider
  :ensure t)

;;
(use-package which-key
  :ensure t
  :config (which-key-mode)
  (setq which-key-idle-delay 0.3)
  (setq which-key-show-remaining-keys t)
  (setq which-key-side-window-location 'bottom))

;;
(use-package org
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-directory "~/org/")
  (setq org-agenda-files (list "~/org/notes.org")))

;; enable org mode indent by default
(add-hook 'org-mode-hook 'org-indent-mode)

;;
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿"))
  (org-superstar-special-todo-items t))

;;
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org/roam")
  :config
  (org-roam-setup))

;;
(use-package flycheck
  :ensure t)

(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package lsp-mode
  :ensure t
  :hook
  (go-mode . lsp-deferred)
  (clojure-mode . lsp-deferred)
  (rustic-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred)
  :config (progn
	    (setq lsp-prefer-flymake nil)
	    (setq lsp-headerline-breadcrumb-enable nil)
	    (setq lsp-lens-enable nil)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config (progn
	    ;; disable inline documentation
	    (setq lsp-ui-sideline-enable nil)
	    ;; disable showing docs on hover at the top of the window
	    (setq lsp-ui-doc-enable nil)))

;; Clojure
(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode . lsp-deferred)
	 (before-save . lsp-format-buffer)
	 (before-save . lsp-organize-imports)))

;; Rust
(use-package rustic
  :ensure t
  ;; (setq rustic-format-on-save t)
  :hook ((rustic-mode . lsp-deferred)
	 (before-save . lsp-format-buffer)
	 (before-save . lsp-organize-imports)))

;; Go
(use-package go-mode
  :ensure t
  :config
  (setq-default tab-width 4)
  :hook ((go-mode . lsp-deferred)
	 (before-save . lsp-format-buffer)
	 (before-save . lsp-organize-imports)))

(use-package corfu
  :init
  (global-corfu-mode))

(add-hook 'lsp-mode-hook #'corfu-mode)

;;
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :defer t)

;;
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;;
(use-package treemacs
  :ensure t
  :defer t)

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package yaml-mode
  :ensure t)

;;; init.el ends here
