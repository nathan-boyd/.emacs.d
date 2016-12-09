
;;; Commentary:
;;; Code:

;; add to path

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
    (setq exec-path (append exec-path '("/usr/local/bin")))

;;;;;;;;;;;;;;;;;;;;
;; setup encoding ;;
;;;;;;;;;;;;;;;;;;;;
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

;; Turn off active processes exist notification
(add-hook 'comint-exec-hook 
      (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

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
(set-face-attribute 'default nil :family "Inconsolata" :height 140)
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

;; change the behavior of editing selected text
(delete-selection-mode t)
(transient-mark-mode t)

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
(setq whitespace-display-mappings '(
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

;;setup packages
(load-library "~/.emacs.d/setup/setup-packages.el")
(load-library "~/.emacs.d/setup/my-modes.el")

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

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
 '(trailing-whitespace ((t (:background "#4F4F4F"))))
 '(whitespace-empty ((t (:background "#4F4F4F"))))
 '(whitespace-indentation ((t (:foreground "dim gray" :background "#3F3F3F"))))
 '(whitespace-line ((t nil)))
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
 '(flycheck-javascript-standard-executable "C:/Program Files/nodejs/standard.cmd")
 '(flycheck-jscsrc "~/.jscsrc")
 '(flycheck-jshintrc "~.jshintrc")
 '(global-whitespace-mode t)
 '(helm-buffer-max-length 80)
 '(js-indent-level 2)
 '(whitespace-line-column 500000)
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 )

;; clear mini buffer
(call-interactively (global-key-binding "\C-g"))

(provide 'init)

;;; init.el ends here
(put 'upcase-region 'disabled nil)
