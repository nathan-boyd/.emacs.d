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
             ac-js2
             aggressive-indent
             auto-compile
             beacon
             benchmark-init
             bm
             company
             company-tern
             csharp-mode
             feature-mode
             flycheck
             flycheck-pos-tip
             flyspell-lazy
             golden-ratio
             helm
             helm-core
             helm-flyspell
             helm-ls-git
             helm-projectile
             highlight-parentheses
             js2-mode
             js2-refactor
             json-mode
             json-reformat
             magit
             markdown-mode
             markdown-preview-mode
             neotree
             omnisharp
             packed
             pkg-info
             popup
             powerline
             saveplace
             smartparens
             smart-mode-line
             undo-tree
             yaml-mode
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

;;;;;;;;;;;;;;;;;;;;;;;
;; setup beacon mode ;;
;;;;;;;;;;;;;;;;;;;;;;;
(beacon-mode 1)

;;;;;;;;;;;;;;;;
;; setup helm ;;
;;;;;;;;;;;;;;;;
(global-unset-key (kbd "C-x c"))

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
(global-set-key (kbd "C-<f6>")     'helm-ls-git-ls)
(global-set-key (kbd "C-x C-d")    'helm-browse-project)
(global-set-key (kbd "C-c h") 'helm-command-prefix)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)   ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action)              ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(helm-mode 1)

;;;;;;;;;;;;;;;;;;;;
;; setup js2-mode ;;
;;;;;;;;;;;;;;;;;;;;
(load-library "~/.emacs.d/setup/setup-js2-mode.el")

;;;;;;;;;;;;;;;;;;;;
;; setup flycheck ;;
;;;;;;;;;;;;;;;;;;;;
(load-library "~/.emacs.d/setup/setup-flycheck.el")

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup look and feel ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'zenburn t)

;;;;;;;;;;;;;;;;;;;
;; shell updates ;;
;;;;;;;;;;;;;;;;;;;
(add-hook 'shell-mode-hook
      #'(lambda ()
          (global-set-key (kbd "M-RET") 'ace-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure undo-tree ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;
;; configure recentf ;;
;;;;;;;;;;;;;;;;;;;;;;;
(require 'recentf)
(setq recentf-auto-cleanup 'never)
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
(powerline-default-theme)
(require 'smart-mode-line)
(setq sml/mode-width 0)
(setq sml/name-width 20)
(rich-minority-mode 1)
(setf rm-blacklist "")
(sml/setup)

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
(setq ispell-program-name "aspell")
(add-to-list 'exec-path "/usr/local/bin")

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (mode '(prog-mode-hook))
  (add-hook mode
            '(lambda ()
               (flyspell-prog-mode))))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup company mode  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(global-company-mode)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-tern)
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

(provide 'setup-packages)

;;; setup-packages ends here
