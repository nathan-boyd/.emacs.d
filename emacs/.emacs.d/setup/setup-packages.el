;;; package --- Summary
;;; Commentary:
;;;     setup packages
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;
;; install packages ;;
;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(
    ("melpa" . "http://melpa.org/packages/")
    ("elpa" . "http://tromey.com/elpa/")
    ("gnu" . "http://elpa.gnu.org/packages/")
    ("marmalade" . "http://marmalade-repo.org/packages/")))

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
                     editorconfig
                     feature-mode
                     flycheck
                     flycheck-pos-tip
                     flyspell-lazy
                     golden-ratio
                     helm
                     helm-flyspell
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
                     tern
                     tfs
                     undo-tree
                     web-beautify
                     web-mode
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

;;;;;;;;;;;;;;;;
;; setup helm ;;
;;;;;;;;;;;;;;;;
(require 'helm-config)
(helm-adaptive-mode t)
(helm-autoresize-mode t)
(helm-push-mark-mode t)
(global-set-key (kbd "M-x")        'undefined)
(global-set-key (kbd "M-x")        'helm-M-x)
(global-set-key (kbd "C-x r b")    'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f")    'helm-find-files)
(global-set-key (kbd "C-x b")      'helm-mini)
(global-set-key (kbd "C-x C-b")    'helm-mini)
(global-set-key (kbd "M-y")        'helm-show-kill-ring)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(helm-mode 1)

;;;;;;;;;;;;;;;;;;;;
;; setup js2-mode ;;
;;;;;;;;;;;;;;;;;;;;
(load-library "~/.emacs.d/setup/setup-js2-mode.el")

;;;;;;;;;;;;;;;;;
;; setup tern  ;;
;;;;;;;;;;;;;;;;;
;(load-library "~/.emacs.d/setup/setup-tern.el")

;;;;;;;;;;;;;;;;;;;;;
;; setup flycheck  ;;
;;;;;;;;;;;;;;;;;;;;;
(load-library "~/.emacs.d/setup/setup-flycheck.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup web-mode for jsx files ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "*.js[x]?\\'")))

;; for better jsx syntax-highlighting in web-mode
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

;; setup web-beautify
(eval-after-load 'sgml-mode
      '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

;; setup smartparens
(require 'smartparens-config)
(smartparens-global-mode 1)
(defun my-after-init-hook ()
  (use-package smartparens-config
    :ensure smartparens
    :config
    (progn
      (show-smartparens-global-mode t)))
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
)

(add-hook 'after-init-hook 'my-after-init-hook)
(sp-pair "<" ">" :wrap "C->")

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
(ac-linum-workaround) ;; fix for linums in ac

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

;;;;;;;;;;;;;;;;;;;;;;;;
;; setup editorconfig ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(require 'editorconfig)
(editorconfig-mode 1)

(setq neo-theme 'ascii)
(custom-set-faces
 '(neo-banner-face ((t . (:inherit shadow))) t)
 '(neo-header-face ((t . (:inherit shadow))) t)
 '(neo-root-dir-face ((t . (:inherit link-visited :underline nil))) t)
 '(neo-dir-link-face ((t . (:inherit dired-directory))) t)
 '(neo-file-link-face ((t . (:inherit default))) t)
 '(neo-button-face ((t . (:inherit dired-directory))) t)
 '(neo-expand-btn-face ((t . (:inherit button))) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure golden-ratio ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'golden-ratio)
(golden-ratio-mode 1)
(setq golden-ratio-auto-scale t)
(add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
(global-set-key [f8] 'neotree-toggle)

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

(provide 'setup-packages)

;;; setup-packages ends here
