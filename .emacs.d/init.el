;;; Package --- Summary Emacs Init File
;;; Commentary:
;;; Code:

(package-initialize)

;; setup encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

;; Turn off active processes exist notification
(require 'cl-lib)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

;; initial look and feel
(menu-bar-mode -1)
(tool-bar-mode -1)
(mouse-wheel-mode t)
(scroll-bar-mode -1)
(mouse-avoidance-mode 'banish)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(set-default 'cursor-type 'bar)
(setq mac-allow-anti-aliasing t)
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

;; dont create new workspace on app start
;; (setq ns-use-native-fullscreen nil)

;; set font
(set-face-attribute 'default nil :family "Inconsolata" :height 160)

;; turn on columns
(setq column-number-mode t)

;; turn on line numbering and format
(defvar linum-format)
(setq linum-format "%d")
(global-linum-mode t)

;; turn off audible bell notification
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; define shell type
(setq explicit-shell-file-name "/bin/bash")

;; change the behavior of editing selected text
(delete-selection-mode t)
(transient-mark-mode t)

;; allow uppercase and lowercase
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; set dired to auto refresh
(add-hook 'dired-mode-hook 'auto-revert-mode)
(global-auto-revert-mode t)

;; run in server mode
(require 'server)
(when (and (>= emacs-major-version 23) (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir)
    "Noop.  DIR." t))

(unless (server-running-p)
  (server-start))

(setq delete-by-moving-to-trash nil)

;; let other packages handle vertical alignment
(electric-indent-mode -1)

;; swap yes for y
(fset 'yes-or-no-p 'y-or-n-p)

;; treat new buffers as text
(setq major-mode 'text-mode)

;; load prefers the newest version of a file
(setq load-prefer-newer t)

;; setup whitespace
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-display-mappings '(
  (space-mark 32 [183] [46])   ; 32 SPACE 「 」
  (newline-mark 10 [182 10])   ; 10 LINE FEED
  (tab-mark 9 [187 9] [92 9])  ; 9  TAB
))

;; (add-hook 'before-save-hook 'whitespace-cleanup)

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
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
(savehist-mode 1)

;; disable lock files
(setq create-lockfiles nil)

;; turn off warnings about ad-redefinition
(setq ad-redefinition-action 'accept)

;; get rid of scratch buffer message
(setq initial-scratch-message "")

;; turn on syntax highlight everywhere
(global-font-lock-mode 1)

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-banner-face ((t :inherit shadow)))
 '(neo-button-face ((t :inherit dired-directory)))
 '(neo-dir-link-face ((t :inherit dired-directory)))
 '(neo-expand-btn-face ((t nil)))
 '(neo-file-link-face ((t :inherit default)))
 '(neo-header-face ((t :inherit shadow)))
 '(neo-root-dir-face ((t :inherit link-visited :underline nil))))

;; start moving configs out of custom set faces and into individual configs
(set-face-attribute 'whitespace-line nil)
(set-face-attribute 'whitespace-tab nil         :foreground "dim gray":background "#3F3F3F")
(set-face-attribute 'whitespace-space nil       :foreground "dim gray":background "#3F3F3F")
(set-face-attribute 'whitespace-newline nil     :foreground "dim gray":background "#3F3F3F")
(set-face-attribute 'whitespace-indentation nil :foreground "dim gray":background "#3F3F3F")

(set-face-attribute 'trailing-whitespace nil         :foreground "dim gray":background "#4F4F4F")
(set-face-attribute 'whitespace-empty nil            :foreground "dim gray":background "#4F4F4F")
(set-face-attribute 'whitespace-space-after-tab nil  :foreground "dim gray":background "#4F4F4F")
(set-face-attribute 'whitespace-space-before-tab nil :foreground "dim gray":background "#4F4F4F")
(set-face-attribute 'whitespace-trailing nil         :foreground "dim gray":background "#4F4F4F")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-completion-addsuffix t)
 '(comint-completion-autolist t)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output t)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(flycheck-ruby-rubocop-executable "~/.rvm/gems/ruby-2.2.2/bin/rubocop")
 '(grep-command "grep -rin --color=auto ")
 '(js-indent-level 2)
 '(markdown-command "/usr/local/bin/markdown")
 '(whitespace-line-column 500000))

;; clear mini buffer
(call-interactively (global-key-binding "\C-g"))

(provide 'init)

;;; init.el ends here
