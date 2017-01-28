;;; package --- Summary
;;; Commentary:
;;;     setup packages
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;
;; install packages ;;
;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install these packages manually ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar package-list)
(setq package-list '(feature-mode
                     packed
                     pkg-info
                     use-package))

(require 'package)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(dolist (package package-list)
  (require 'package))

;;;;;;;;;;;;;;;;;
;; apply theme ;;
;;;;;;;;;;;;;;;;;
(use-package zenburn-theme
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install and configure with use-package ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package auto-compile
  :ensure t
  :init
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent
  :config
  ;; (global-aggressive-indent-mode 1)
  )

(use-package ace-window
  :ensure t
  :init
    (setq aw-scope 'frame)
    (global-set-key (kbd "M-RET") 'ace-window)
    (add-hook 'shell-mode-hook
      #'(lambda ()
        (define-key shell-mode-map (kbd "M-RET") 'ace-window))))

;; (use-package beacon
;;   :ensure t
;;   :diminish beacon
;;   :config (beacon-mode 1))

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-to-list 'company-backends 'company-tern)
  (add-to-list 'company-backends 'company-omnisharp)
  (setq company-tern-meta-as-single-line t)            ; trim too long function signatures to the frame width.
  (setq company-tooltip-limit 15)                      ; bigger popup window
  (setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
  (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  :bind
  (("C-<return>" . company-complete)
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-d" . company-show-doc-buffer)
   ("M-." . company-show-location))
  :config
  (global-company-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)
                        '(javascript-eslint)
                        '(javascript-gjslint)
                        '(javascript-jscs)))
  (flycheck-add-mode 'javascript-standard 'js2-mode)
  (flycheck-add-mode 'javascript-standard 'js-mode)
  (flycheck-add-mode 'javascript-standard 'web-mode)
  (setq-default flycheck-temp-prefix ".flycheck")
  :config
  (global-flycheck-mode))

(use-package flyspell
  :ensure t
  :diminish flyspell-mode
  :init
  (setq ispell-program-name "aspell")
  :config
  (flyspell-mode-1)
  (flyspell-prog-mode))

(use-package golden-ratio
  :ensure t
  :init
    (golden-ratio-mode 1)
    (add-to-list 'golden-ratio-extra-commands 'ace-window))

(use-package helm-flyspell
  :ensure t)

(use-package helm-dash
  :ensure t
  :config
  (setq helm-dash-browser-func 'eww)
  (setq helm-dash-docsets-path (format "%s/.emacs.d/docsets" (getenv "HOME")))
  (setq dash-common-docsets '("Emacs Lisp" "NodeJS" "NET_Framework")))

(use-package helm
  :ensure t
  :init
  (setq helm-buffer-max-length 80)
  (helm-adaptive-mode t)
  (helm-autoresize-mode t)
  (helm-push-mark-mode t)
  :diminish helm-mode
  :bind (("M-x" . undefined)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-mini)
         ("C-x C-d" . helm-browse-project)
         ("C-c h" . helm-command-prefix)
         ("C-i" . helm-execute-persistent-action)
         ("C-;" . helm-flyspell-correct)
         ("C-z" . helm-select-action))
  :config
  (helm-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)   ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action)              ; list actions using C-z
  (add-hook 'shell-mode-hook
            (lambda ()
              (define-key shell-mode-map [remap pcomplete] 'helm-esh-pcomplete)
              (define-key shell-mode-map (kbd "M-h") 'helm-eshell-history))))

(use-package hlinum
  :ensure t
  :diminish hlinum
  :config (hlinum-activate))

(use-package js2-mode
  :ensure t
  :diminish js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (setq-default indent-tabs-mode nil)
  (setq-default js2-idle-timer-delay 0.1)
  (setq-default js2-indent-on-enter-key nil)
  (setq-default js2-enter-indents-newline nil)
  (setq-default js2-highlight-level 3)
  (setq-default js2-basic-offset 2)
  (setq-default js2-show-parse-errors nil)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-strict-trailing-comma-warning t)
  :config
  (add-hook 'js2-mode-hook 'flycheck-mode))

;; (use-package js2-refactor
;;   :ensure t
;;   :diminish js2-refactor
;;   :config
;;   (js2r-add-keybindings-with-prefix "C-c C-r")
;;   (add-hook 'js2-mode-hook 'js2-refactor-mode))

(use-package json-mode
  :ensure t
  :diminish json-mode)

(use-package json-reformat
  :ensure t)

(use-package neotree
  :ensure t
  :diminish neotree
  :init
  (setq neo-theme 'nerd))

(use-package origami
  :ensure t
  :diminish origami
  :config (global-origami-mode 1))

(use-package omnisharp
  :ensure t
  :diminish omnisharp-mode
  :init
  (setq omnisharp-server-executable-path "/Users/nboyd/git/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

(use-package recentf
  :ensure t
  :init
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-menu-items 25)
  :diminish recentf-mode
  :bind (("C-x \C-r" . recentf-open-files))
  :config (recentf-mode 1))

;; (use-package save-place
;;   :ensure t
;;   :diminish save-place-mode
;;   :config
;;   (save-place-mode))

(use-package smart-mode-line
  :ensure t
  :diminish smart-mode-line
  :init
  (require 'smart-mode-line)
  (setq sml/mode-width 0)
  (setq sml/name-width 20)
  (rich-minority-mode 1)
  (setf rm-blacklist "")
  :config
  (powerline-default-theme)
  (sml/setup))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  :config
  (sp-pair "<" ">" :wrap "C->")
  (smartparens-global-mode 1))

(use-package tern
  :ensure t
  :diminish tern-mode
  :init
  (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  (add-hook 'js2-mode-hook (lambda () (tern-mode t))))

(use-package undo-tree
  :ensure t
  :diminish
  undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package yaml-mode
  :ensure t
  :diminish yamp-mode)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq yas-indent-line (quote none))
  :config
  (yas-global-mode 1))

(provide 'setup-packages)

;;; setup-packages ends here
