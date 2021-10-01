;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name ""
      user-mail-address "")

(setq doom-font (font-spec :family "Iosevka SS04" :size 16 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Fira Code" :size 20))

;; set light or dark theme depending on the time of the day
(defun theme-brightess ()
  (if
      (and
       (< (string-to-number (format-time-string "%H")) 16)
       (> (string-to-number (format-time-string "%H")) 8))
      (setq doom-theme 'doom-flatwhite)
    (setq doom-theme 'doom-plain-dark))
 )

(theme-brightess)

;; start emacs in fullscreen mode
(toggle-frame-fullscreen)

(setq display-line-numbers-type 'relative)

(unless (equal "Battery status not avalaible"
               (battery))
  (display-battery-mode 1))

;; Display current time
(display-time-mode 1)

;; macOs specific settings
(cond (IS-MAC
       (setq mac-command-modifier      'meta
             mac-option-modifier       'alt
             mac-right-option-modifier 'alt)))
;;
;; lsp-ui
(after! lsp-ui
  (setq lsp-ui-doc-enable 1
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-use-childframe nil
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-peek-enable t
        lsp-ui-flycheck-enable -1)
)

;; org-mode
(setq org-directory "~/org/")

;; org-roam
(setq org-roam-directory "~/org/roam")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Languages
;;
;; Clojure
(map! :map clojure-mode-map
      :after clojure-mode
      :n "M-<RET>" #'cider-eval-sexp-at-point)

;; Rust
(setq rustic-lsp-server 'rust-analyzer)
(add-hook 'before-save-hook
          (lambda ()
            (when
                (eq 'rustic-mode major-mode)
              (lsp-format-buffer))))

;; Go
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org
(setq org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿"))
(after! org
  (setq
   org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
   org-todo-keyword-faces
   '(("TODO" :foreground "#7c7c75" :weight normal :underline t)
     ("WAITING" :foreground "#9f7efe" :weight normal :underline t)
     ("INPROGRESS" :foreground "#0098dd" :weight normal :underline t)
     ("DONE" :foreground "#50a14f" :weight normal :underline t)
     ("CANCELLED" :foreground "#ff6480" :weight normal :underline t))
   org-priority-faces '((65 :foreground "#e45649")
                        (66 :foreground "#da8548")
                        (67 :foreground "#0098dd"))
   ))
