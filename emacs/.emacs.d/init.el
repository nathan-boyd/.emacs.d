;;; Package --- Summary Emacs Init File
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;
;; setup encoding ;;
;;;;;;;;;;;;;;;;;;;;
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Turn off active processes exist notification
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

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
(set-face-attribute 'default nil
                    :family "Consolas" :height 115)
(w32-send-sys-command 61488)

;; use windows trash
(setq delete-by-moving-to-trash t)

;; change the behavior of editing selected text
(delete-selection-mode t)
(transient-mark-mode t)

;; use default clipboard
(setq x-select-enable-clipboard t)

;; allow downcase-region command
(put 'downcase-region 'disabled nil)

;; set dired to auto refresh
(add-hook 'dired-mode-hook 'auto-revert-mode)
(global-auto-revert-mode t)

;; run in server mode
(require 'server)
(when (and (>= emacs-major-version 23)
           (equal window-system 'w32))
(defun server-ensure-safe-dir (dir) "Noop" t))
(setq delete-by-moving-to-trash nil)

(unless (server-running-p)
  (server-start))

;; let smart tabs handle vertical alignment
(electric-indent-mode -1)

;; swap yes for y
(fset 'yes-or-no-p 'y-or-n-p)

;; treat new buffers as text
(setq major-mode 'text-mode)

;; setup whitespace
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-display-mappings
      '(
        (space-mark 32 [183] [46])   ; 32 SPACE 「 」
        (newline-mark 10 [182 10])   ; 10 LINE FEED
        (tab-mark 9 [187 9] [92 9])  ; 9  TAB
        ))

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

;; highlight current line
;; (global-hl-line-mode 1)

;; setup line numbers
;; (global-linum-mode 1)  ;; line numbers causing slow cursor

;; show clock
(display-time-mode 1)

;; setup UNIX utils (support for grep mostly) for windows
(when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
  (setenv "PATH" (concat "D:/apps/unixUtils/usr/local/wbin;" (getenv "PATH"))))

;;setup packages
(load-library "~/.emacs.d/setup/setup-packages.el")
(load-library "~/.emacs.d/setup/my-modes.el")
(load-library "~/.emacs.d/setup/setup-sql.el")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-buffer-directory ((t (:foreground "slate gray"))))
 '(helm-buffer-size ((t (:foreground "slate gray"))))
 '(neo-banner-face ((t :inherit shadow)))
 '(neo-button-face ((t :inherit dired-directory)))
 '(neo-dir-link-face ((t :inherit dired-directory)))
 '(neo-expand-btn-face ((t nil)))
 '(neo-file-link-face ((t :inherit default)))
 '(neo-header-face ((t :inherit shadow)))
 '(neo-root-dir-face ((t :inherit link-visited :underline nil)))
 '(whitespace-empty ((t (:background "#4F4F4F"))))
 '(whitespace-indentation ((t (:foreground "dim gray" :background "#3F3F3F"))))
 '(whitespace-newline ((t (:bold t :foreground "dim gray" :background "#3F3F3F"))))
 '(whitespace-space ((t (:bold t :foreground "dim gray" :background "#3F3F3F"))))
 '(whitespace-space-after-tab ((t (:background "#4F4F4F" :foreground "#4F4F4F"))))
 '(whitespace-space-before-tab ((t (:background "#4F4F4F" :foreground "#4F4F4F"))))
 '(whitespace-tab ((t (:bold t :foreground "dim gray" :background "#3F3F3F"))))
 '(whitespace-trailing ((t (:background "#4F4F4F" :foreground "#4F4F4F")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-eslintrc "~/.eslintrc")
 '(flycheck-jscsrc "~/.jscsrc")
 '(flycheck-jshintrc "~.jshintrc")
 '(whitespace-line-column 500))

;; clear mini buffer
(call-interactively (global-key-binding "\C-g"))

(provide 'init)

;;; init.el ends here
