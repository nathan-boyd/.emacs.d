;;; Package --- Summary Emacs Init File
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;
;; setup emacs ;;
;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initial look and feel ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mouse-wheel-mode t)
(mouse-avoidance-mode 'banish)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'cursor-type 'bar)
(setq column-number-mode t)
(add-hook 'after-init-hook 'toggle-frame-maximized)
(add-hook 'after-init-hook 'toggle-frame-fullscreen)
(w32-send-sys-command 61488)

;; allow downcase-region command
(put 'downcase-region 'disabled nil)

; run in server mode
(require 'server)
(when (and (>= emacs-major-version 23)
           (equal window-system 'w32))
(defun server-ensure-safe-dir (dir) "Noop" t))

(unless (server-running-p)
  (server-start))

;; let smart tabs handle vertical alignment
(electric-indent-mode -1)

;; swap yes for y
(fset 'yes-or-no-p 'y-or-n-p)

;; treat new buffers as text
(setq major-mode 'text-mode)

;; ;; setup whitespace
(require 'whitespace)
(global-whitespace-mode 1)
;(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
(setq whitespace-display-mappings
      '(
        (space-mark 32 [183] [46])   ; 32 SPACE 「 」
        (newline-mark 10 [182 10])   ; 10 LINE FEED
        (tab-mark 9 [187 9] [92 9])  ; 9  TAB
        ))

; set whitespace-space background to match zenburn theme
(custom-set-faces
'(whitespace-space ((t (:bold t :foreground "dim gray" :background "#3F3F3F"))))
)

; configure backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t
  kept-new-versions 5
  kept-old-versions 5
  version-control t)
(setq vc-make-backup-files t)

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

; setup save history
(eval-when-compile (defvar savehist-file))
(eval-when-compile (defvar savehist-save-minibuffer-history))
(eval-when-compile (defvar savehist-additional-variables))
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; disable lock files
(setq create-lockfiles nil)

;; turn off warnings about ad-redefinition
(setq ad-redefinition-action 'accept)

;; get rid of scratch buffer message
(setq initial-scratch-message "")

;; turn on syntax highlight everywhere
(global-font-lock-mode 1)

;; setup line numbers ;;
(global-linum-mode 1)

;; show clock
(display-time-mode 1)

;; setup UNIX utils (support for grep mostly) for windows
(when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
  (setenv "PATH" (concat "D:/apps/unixUtils/usr/local/wbin;" (getenv "PATH"))))

;;setup packages
(load-library "~/.emacs.d/setup/setup-packages.el")
(load-library "~/.emacs.d/lib/my-modes.el")
(load-library "~/.emacs.d/lib/setup-sql.el")

(custom-set-variables
 '(flycheck-jshintrc "~.jshintrc")
 '(flycheck-jscsrc   "~/.jscsrc")
 '(flycheck-eslintrc "~/.eslintrc"))

;; clear mini buffer
(call-interactively (global-key-binding "\C-g"))

(provide 'init)

;;; init.el ends here
