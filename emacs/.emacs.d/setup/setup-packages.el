;;; package --- Summary
;;; Commentary:
;;;     setup packages
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;
;; install packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a list of packages to be installed ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                     bm
                     company
                     csharp-mode
                     flycheck
                     flycheck-pos-tip
                     flyspell-lazy
                     golden-ratio
                     helm
                     helm-c-yasnippet
                     helm-core
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
                     neotree
                     saveplace
                     smartparens
                     smart-mode-line
                     smart-tabs-mode
                     solarized-theme
                     sublimity
                     tfs
                     undo-tree
                     which-key
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

;;;;;;;;;;;;;;;;;;;;
;; setup js2-mode ;;
;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;;;;;;;;;;;;;;;;;;;
;; setup flycheck ;;
;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook #'global-flycheck-mode)

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; override js2 error checking ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning nil)

;;;;;;;;;;;;;;;;;;;;;
;; setup which-key ;;
;;;;;;;;;;;;;;;;;;;;;
(which-key-mode)
(which-key-setup-side-window-right)

;;;;;;;;;;;;;;
;; autopair ;;
;;;;;;;;;;;;;;
(autopair-global-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup look and feel ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'zenburn t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure omnisharp csharp integration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "~/.emacs.d/setup/setup-omnisharp.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup cursor highlighting ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(beacon-mode 1)

;;;;;;;;;;;;;;;;;;;
;; setup origami ;;
;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fix for linums in ac ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(ac-linum-workaround)

;;;;;;;;;;;;;;;;
;; setup helm ;;
;;;;;;;;;;;;;;;;
(require 'helm-config)
(helm-mode 1)
(helm-adaptive-mode t)
(helm-autoresize-mode t)
(helm-push-mark-mode t)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)   ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)     ; make TAB work in terminal
;;(define-key helm-map (kbd "C-z")  'helm-select-action)                ; list actions using C-z

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
              (define-key eshell-mode-map (kbd "M-h") 'helm-eshell-history)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure undo-tree ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;
;; configure recentf ;;
;;;;;;;;;;;;;;;;;;;;;;;
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
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
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(yas-global-mode 1)
(setq yas-indent-line (quote none))

;; configure helm-c-yasnippet
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
(add-to-list 'exec-path "D:/apps/hunspell/bin")
(setq ispell-program-name "hunspell")
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure golden-ratio ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'golden-ratio)
(golden-ratio-mode 1)
(setq golden-ratio-auto-scale t)

;;;;;;;;;;;;;;;;;;;
;; configure tfs ;;
;;;;;;;;;;;;;;;;;;;

;; this has to be run manually rather than in the package-list loop
(require 'tfs)
(setq tfs/tf-exe "D:/apps/VisualStudio/Common7/IDE/tf.exe")

;;;;;;;;;;;;;;;;
;; setup tabs ;;
;;;;;;;;;;;;;;;;
(load "~/.emacs.d/setup/setup-tabs.el")

(require 'sublimity)
(require 'sublimity-scroll)
(require 'sublimity-map)

(provide 'setup-packages)

;;; setup-packages ends here
