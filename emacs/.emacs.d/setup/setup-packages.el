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
             ace-window
             aggressive-indent
             auto-compile
             beacon
             benchmark-init
             bm
             company
             company-edbi
             company-tern
             csharp-mode
             edbi
             edbi-minor-mode
             editorconfig
             feature-mode
             flycheck
             flycheck-pos-tip
             flyspell-lazy
             golden-ratio
             helm
             helm-flyspell
             helm-core
             helm-projectile
             highlight-parentheses
             js2-mode
             js2-refactor
             json-mode
             json-reformat
             magit
             neotree
             origami
             omnisharp
             packed
             pkg-info
             popup
             powerline
             projectile
             restclient
             saveplace
             smartparens
             smart-mode-line
             solarized-theme
             sublimity
             tern
             tfs
             undo-tree
             web-beautify
             web-mode
             which-key
             yasnippet
             zenburn-theme))

(require 'package)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; install packages that aren't already installed
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(dolist (package package-list)
  (require 'package))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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

;;;;;;;;;;;;;;;;;;;;;;;;
;; non-trivial setups ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;; setup js2-mode ;;
;;;;;;;;;;;;;;;;;;;;
(load-library "~/.emacs.d/setup/setup-js2-mode.el")

;;;;;;;;;;;;;;;;;
;; setup tern  ;;
;;;;;;;;;;;;;;;;;
(load-library "~/.emacs.d/setup/setup-tern.el")

;;;;;;;;;;;;;;;;;;;;;
;; setup flycheck  ;;
;;;;;;;;;;;;;;;;;;;;;
(load-library "~/.emacs.d/setup/setup-flycheck.el")

;;;;;;;;;;;;;;;;;;;;
;; trivial setups ;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;
;; setup projectile ;;
;;;;;;;;;;;;;;;;;;;;;;
(projectile-global-mode)
(setq projectile-enable-caching t)
(helm-projectile-on)

;;;;;;;;;;;;;;;;;;;;;
;; setup sublimity ;;
;;;;;;;;;;;;;;;;;;;;;
(require 'sublimity)
(require 'sublimity-scroll)
(require 'sublimity-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure syntax highlighting for jsx in web-mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;
;; setup smartparens ;;
;;;;;;;;;;;;;;;;;;;;;;;
(require 'smartparens-config)
(smartparens-global-mode 1)
(defun smartParens-after-init-hook ()
  (use-package smartparens-config
               :ensure smartparens
               :config
               (progn
                 (show-smartparens-global-mode t)))
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode))

(add-hook 'after-init-hook 'smartParens-after-init-hook)
(sp-pair "<" ">" :wrap "C->")

;;;;;;;;;;;;;;;;;;;;;
;; setup which-key ;;x
;;;;;;;;;;;;;;;;;;;;;
(which-key-mode)
(which-key-setup-side-window-right)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup origami, code folding package ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-origami-mode)
(global-set-key (kbd "C--") 'origami-toggle-node)

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; save place in files ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

;;;;;;;;;;;;;;;;;;;;;
;; setup powerline ;;
;;;;;;;;;;;;;;;;;;;;;
;(powerline-default-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure yasnippet ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(yas-global-mode 1)
(setq yas-indent-line (quote none))

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
(global-set-key (kbd "M-RET") 'ace-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup spell checking ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'exec-path "D:/apps/hunspell/bin")
(setq ispell-program-name "hunspell")
(add-hook 'text-mode-hook 'flyspell-mode)(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct))

;;;;;;;;;;;;;;;;;;;;;;;;
;; setup editorconfig ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(require 'editorconfig)
(editorconfig-mode 1)

;;;;;;;;;;;;;;;;;;;
;; setup neotree ;;
;;;;;;;;;;;;;;;;;;;
(setq neo-theme 'nerd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure golden-ratio ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'golden-ratio)
(golden-ratio-mode 1)
(setq golden-ratio-auto-scale t)
(add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)

;;;;;;;;;;;;;;;;;;;
;; configure tfs ;;
;;;;;;;;;;;;;;;;;;;
;; this has to be run manually rather than in the package-list loop
(require 'tfs)
(setq tfs/tf-exe "D:/apps/visualStudio2013/Common7/IDE/tf.exe")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup company mode  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(global-company-mode)

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-tern)
  (add-to-list 'company-backends 'company-edbi)
  (add-to-list 'company-backends 'company-omnisharp)
  (setq company-tern-meta-as-single-line t)                                    ; trim too long function signatures to the frame width.
  (setq company-tooltip-limit 15)                                              ; bigger popup window
  (setq company-tooltip-align-annotations 't)                                  ; align annotations to the right tooltip border
  (setq company-idle-delay .3)                                                 ; decrease delay before autocompletion popup shows
  (setq company-begin-commands '(self-insert-command))                         ; start autocompletion only after typing
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location)
  (global-set-key (kbd "C-<return>") 'company-complete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure edbi-minor-mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'sql-mode-hook 'edbi-minor-mode)

(provide 'setup-packages)

;;; setup-packages ends here
