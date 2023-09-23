;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;; My Emacs settings

;;; Code:
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

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

;;
(setq inhibit-startup-message t)

(setq user-full-name "Nino Paparo")

;; Avoid opening UI pop up dialogs
(setq use-dialog-box nil)
;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(defvar user-org-config-directory "~/.emacs.d/")
(defvar my-org-config (concat  user-org-config-directory "my-conf.org"))

(org-babel-load-file (expand-file-name my-org-config))

;;; init.el ends here
