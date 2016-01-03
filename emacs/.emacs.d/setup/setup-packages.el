;;; package --- Summary
;;; Commentary:
;;;     setup packages
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;
;; install packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(setq package-archives '(
                         ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

; a list of packages to be installed
(defvar package-list)
(setq package-list '(
                     ac-helm
                     ac-js2
                     ace-window
                     aggressive-indent
                     auto-compile
                     auto-complete
                     autopair
                     beacon
                     benchmark-init
                     browse-kill-ring
                     bm
                     company
                     csharp-mode
                     elscreen
                     flycheck
                     flycheck-pos-tip
                     flyspell-lazy
                     golden-ratio
                     helm
                     helm-c-yasnippet
                     helm-core
                     helm-flycheck
                     helm-flyspell
                     highlight-parentheses
                     js2-mode
                     js2-refactor
                     json-mode
                     json-reformat
                     origami
                     omnisharp
                     packed
                     pkg-info
                     popup
                     powerline
                     restclient
                     saveplace
                     smartparens
                     smart-mode-line
                     smart-tabs-mode
                     solarized-theme
                     tern
                     tern-auto-complete
                     tfs
                     undo-tree
                     which-key
                     workgroups2
                     yasnippet
                     zenburn-theme
                     ))

(require 'package)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(dolist (package package-list)
  (require 'package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi screen support with elscreen ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (elscreen-start)

;;;;;;;;;;;;;;;;;;;;;
;; setup which-key ;;
;;;;;;;;;;;;;;;;;;;;;
(which-key-mode)
(which-key-setup-side-window-right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup aggressive-indent-mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-aggressive-indent-mode 1)

;;;;;;;;;;;;;;
;; autopair ;;
;;;;;;;;;;;;;;
(autopair-global-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup look and feel ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'zenburn t)

;;;;;;;;;;;;;;;;;;;;;;
;; setup workgroups ;;
;;;;;;;;;;;;;;;;;;;;;;
;; (setq wg-session-file "~/.emacs.d/.workgroups")
;; (setq wg-emacs-exit-save-behavior 'nil)
;; (workgroups-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure omnisharp ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "~/.emacs.d/setup/setup-omnisharp.el")

;;;;;;;;;;;;;;;;;;
;; setup beacon ;;
;;;;;;;;;;;;;;;;;;
(beacon-mode 1)

;;;;;;;;;;;;;;;;;;;
;; setup origami ;;
;;;;;;;;;;;;;;;;;;;
(require 'origami)
(global-origami-mode)
(global-set-key (kbd "C--") 'origami-toggle-node)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure auto-complete ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-delay 0.1)
(setq ac-quick-help-delay 0.1)
(setq ac-sources '(ac-source-semantic ac-source-yasnippet))
(add-to-list 'ac-modes 'sql-mode)
(add-to-list 'ac-modes 'csharp-mode)
(ac-set-trigger-key "TAB")

;;;;;;;;;;;;;;;;
;; setup helm ;;
;;;;;;;;;;;;;;;;
(require 'helm-config)
(helm-mode 1)
(helm-adaptive-mode t)
(helm-autoresize-mode t)
(helm-push-mark-mode t)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;;;;;;;;;;;;;;;;;;;;;;;
;; configure ac-helm ;;
;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-.") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-.") 'ac-complete-with-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add helm support for eshell ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure browse-kill-ring ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(browse-kill-ring-default-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure undo-tree ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;
;; configure recentf ;;
;;;;;;;;;;;;;;;;;;;;;;;
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 100)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save the place in files ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

;;;;;;;;;;;;;;;;;;;;;
;; setup powerline ;;
;;;;;;;;;;;;;;;;;;;;;
(powerline-default-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure yasnippet ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(yas-global-mode 1)
(setq yas-indent-line (quote none))
(setq yas-snippet-dirs '("~/.emacs.d/lib/yasnippets"
                         "D:/gitHubRepos/nbtools/snippets/emacs/yas"))
(yas-reload-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure helm-c-yasnippet ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup highlight-parentheses ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;;;;;;;;;;;;;;;;;;;;;;
;; setup ace-window ;;
;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-p") 'ace-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup spell checking ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'exec-path "D:\\apps\\hunspell\\bin")
(setq ispell-program-name "hunspell")
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure flycheck syntax checking ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

; disable eslint checker
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-eslint)))

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; configure js2-mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; set js2 as major mode for js files
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; add minor modes for js files
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(setq-default js2-auto-indent-p nil)
(setq-default js2-global-externs '("module" "require" "assert" "setTimeout" "setInterval" "console" "JSON"))
(setq-default js2-idle-timer-delay 0.1)
(setq-default js2-indent-on-enter-key nil)
(setq-default js2-enter-indents-newline nil)
(setq-default js2-highlight-level 3)
(setq-default ac-js2-evaluate-calls t)

;; Let flycheck handle parse errors
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t)

;;;;;;;;;;;;;;;;;;;;;;;
;; setup js2refactor ;;
;;;;;;;;;;;;;;;;;;;;;;;
(js2r-add-keybindings-with-prefix "C-r C-r")
(add-hook 'js2-mode-hook #'js2-refactor-mode)

;;;;;;;;;;;;;;;;
;; setup tern ;;
;;;;;;;;;;;;;;;;
(add-to-list 'load-path "C:/Users/nboyd/AppData/Roaming/npm/node_modules/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;; fix for linums in ac
(ac-linum-workaround)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure golden-ratio ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'golden-ratio)
(golden-ratio-mode 1)
(setq golden-ratio-auto-scale t)

;;;;;;;;;;;;;;;;;;;
;; configure tfs ;;
;;;;;;;;;;;;;;;;;;;
(setq tfs/tf-exe "D:/apps/VisualStudio/Common7/IDE/tf.exe")

;;;;;;;;;;;;;;;;
;; setup tabs ;;
;;;;;;;;;;;;;;;;
(load "~/.emacs.d/setup/setup-tabs.el")

(provide 'setup-packages)

;;; setup-packages ends here
