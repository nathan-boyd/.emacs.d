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
  :bind
    (("M-RET" . ace-window)
  :map shell-mode-map
    ("M-RET" . ace-window))
  :config
    (setq aw-scope 'frame))

(use-package company-tern
  :ensure t)

(use-package company
  :ensure t
  :diminish company-mode
  :config
    (setq company-tooltip-limit 15)                      ; bigger popup window
    (setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
    (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
    (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
    (add-to-list 'company-backends 'company-tern)
    (add-to-list 'company-backends 'company-omnisharp)
    (global-company-mode 1)
  :bind
    (("C-<return>" . company-complete)
  :map company-active-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("C-d" . company-show-doc-buffer)
    ("M-." . company-show-location)))

(use-package exec-path-from-shell
  :ensure t
  :config
    (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (flycheck-add-mode 'javascript-standard 'js2-mode)
    (flycheck-add-mode 'javascript-standard 'js-mode)
    (flycheck-add-mode 'javascript-standard 'web-mode)
    (setq-default flycheck-temp-prefix ".flycheck")
    (global-flycheck-mode)
    (setq-default flycheck-disabled-checkers
      (append flycheck-disabled-checkers
        '(javascript-jshint)
        '(javascript-eslint)
        '(javascript-gjslint)
        '(javascript-jscs)))
    (global-flycheck-mode))

(use-package flyspell
  :ensure t
  :diminish flyspell-mode
  :config
    (setq ispell-program-name "aspell")
    (flyspell-mode 1)
    (flyspell-prog-mode))

(use-package golden-ratio
  :ensure t
  :config
    (golden-ratio-mode 1)
    (add-to-list 'golden-ratio-extra-commands 'ace-window))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
    (helm-mode 1)
  :bind
    (("M-x" . undefined)
     ("M-x" . helm-M-x)
     ("M-y" . helm-show-kill-ring)
     ("C-x C-f" . helm-find-files)
     ("C-h b" . helm-descbinds)
     ("C-x b" . helm-mini)
     ("C-x C-b" . helm-mini)
     ("C-x C-d" . helm-browse-project)
     ("C-c h" . helm-command-prefix)
     ("C-i" . helm-execute-persistent-action)
     ("C-;" . helm-flyspell-correct)
     ("C-z" . helm-select-action)
  :map helm-map
     ("<tab>" . helm-execute-persistent-action)
     ("C-i" . helm-execute-persistent-action)
     ("C-z" .  helm-select-action))
  :config
    (setq helm-buffer-max-length 80)
    (helm-adaptive-mode t)
    (helm-autoresize-mode t)
    (helm-push-mark-mode t))


(use-package helm-flyspell
  :ensure t)

(use-package helm-swoop
  :ensure t
  :bind
    (("C-s" . helm-swoop)
     ("M-i" . helm-swoop)
     ("M-s s" . helm-swoop)
     ("M-s M-s" . helm-swoop)
     ("C-c M-i" . helm-multi-swoop)
     ("C-x M-i" . helm-multi-swoop-all)
  :map isearch-mode-map
     ("M-i" . helm-swoop-from-isearch)
  :map helm-swoop-map
     ("M-i" . helm-multi-swoop-all-from-helm-swoop))
  :config
    (setq helm-swoop-split-with-multiple-windows nil)
    (setq helm-swoop-split-direction 'split-window-vertically))

(use-package hlinum
  :ensure t
  :diminish hlinum
  :config
    (hlinum-activate))

(use-package js2-mode
  :ensure t
  :diminish js2-mode
  :config
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
  (add-hook 'js2-mode-hook 'flycheck-mode))

(use-package json-mode
  :ensure t
  :diminish json-mode)

(use-package json-reformat
  :ensure t)

(use-package neotree
  :ensure t
  :diminish neotree
  :config
    (setq neo-theme 'nerd))

(use-package origami
  :ensure t
  :diminish origami
  :config
    (global-origami-mode 1))

(use-package omnisharp
  :ensure t
  :diminish omnisharp-mode
  :config
    (setq omnisharp-server-executable-path "/Users/nboyd/git/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")
    (add-hook 'csharp-mode-hook 'omnisharp-mode))

(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters
  :config
    (add-hook 'prog-mode-hook (lambda()
                      (rainbow-delimiters-mode t))))

(use-package recentf
  :ensure t
  :diminish recentf-mode
  :bind
    (("C-x \C-r" . recentf-open-files))
  :config
    (setq recentf-auto-cleanup 'never)
    (setq recentf-max-menu-items 25)
    (recentf-mode 1))

;; save last position in file
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)
(if (fboundp #'save-place-mode)
  (save-place-mode +1)
  (setq-default save-place t))

(use-package smart-mode-line
  :ensure t
  :diminish smart-mode-line
  :config
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme 'respectful)
    (setq powerline-arrow-shape 'curve)
    (setq powerline-default-separator-dir '(right . left))
    (rich-minority-mode 1)
    (setf rm-blacklist "")
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
  :diminish t
  :config
    (add-hook 'javascript-hook 'tern-mode)
    (add-hook 'js2-mode-hook 'tern-mode))

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
