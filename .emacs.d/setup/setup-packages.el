;;; package --- Summary
;;; Commentary:
;;;     setup packages
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup package archives ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

;;;;;;;;;;;;;;;;;;;;;;;
;; setup use-package ;;
;;;;;;;;;;;;;;;;;;;;;;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pre-install package config tools ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

;;;;;;;;;;;;;;;;;
;; apply theme ;;
;;;;;;;;;;;;;;;;;
(use-package zenburn-theme
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install and configure package ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package antlr-mode
  :commands (antlr-mode)
  :mode (("\\.g4\\'" . antlr-mode)))

(use-package auto-compile
  :ensure t
  :init
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package aggressive-indent
  :ensure t
  :diminish
    aggressive-indent)

(use-package ace-window
  :ensure t
  :init
    (defvar shell-mode-map)
  :bind
    (("M-RET" . ace-window)
  :map shell-mode-map
    ("M-RET" . ace-window))
  :config
    (setq aw-scope 'frame))

(use-package avy
  :ensure t
  :init
  (setq avy-keys-alist
        `((avy-goto-char-timer . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
          (avy-goto-line . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))))
  (setq avy-style 'pre)
  :bind* (("M-m F" . avy-goto-char-timer)
          ("M-m f" . avy-goto-line)))

;; additions to which-key for avy
(with-eval-after-load "which-key"
  (which-key-add-key-based-replacements
    "F" "find on-screen"
    "f" "find line"))

(use-package company
  :ensure t
  :init
    (defvar company-dabbrev-downcase)
  :diminish company-mode
  :init
    (setq company-minimum-prefix-length 2
          company-require-match 0
          company-selection-wrap-around t
          company-dabbrev-downcase nil
          company-tooltip-limit 20                       ; bigger popup window
          company-tooltip-align-annotations 't           ; align annotations to the right tooltip border
          company-idle-delay .4                          ; decrease delay before autocompletion popup shows
          company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  :bind (("C-<return>" . company-complete)
         :map company-active-map
         ("C-n"        . company-select-next)
         ("C-p"        . company-select-previous)
         ([return]     . company-complete-selection)
         ("C-w"        . backward-kill-word)
         ("C-c"        . company-abort)
         ("C-c"        . company-search-abort))
  :config
  (global-company-mode)
  (add-to-list 'company-backends 'company-omnisharp)
  (add-to-list 'company-backends 'company-css)
  (add-to-list 'company-backends 'company-keywords))

(use-package company-tern
  :ensure t
  :init 
    (add-to-list 'company-backends 'company-tern))

(use-package helm-dash
  :ensure t)

(use-package diff-hl
  :ensure t
  :commands (global-diff-hl-mode
             diff-hl-mode
             diff-hl-next-hunk
             diff-hl-previous-hunk
             diff-hl-mark-hunk
             diff-hl-diff-goto-hunk
             diff-hl-revert-hunk)
  :bind* (("M-m ] h" . diff-hl-next-hunk)
          ("M-m [ h" . diff-hl-previous-hunk)
          ("M-m i h" . diff-hl-mark-hunk)
          ("M-m a h" . diff-hl-mark-hunk)
          ("M-m g h" . diff-hl-diff-goto-hunk)
          ("M-m g H" . diff-hl-revert-hunk))
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  (diff-hl-dired-mode))

;; setup which-key bindings for diff-hl
(with-eval-after-load "which-key"
  (which-key-add-key-based-replacements
  "] h" "next git hunk"
  "[ h" "previous git hunk"
  "g h" "goto git hunk"
  "g H" "revert git hunk"
  "i h" "select git hunk"
  "a h" "select a git hunk"))

(use-package exec-path-from-shell
  :ensure t
  :init
    (defvar exec-path-from-shell-check-startup-files)
    (setq exec-path-from-shell-check-startup-files nil)
  :config
    (exec-path-from-shell-initialize))

(use-package eww
  :bind* (("M-m g x" . eww)
          ("M-m g :" . eww-browse-with-external-browser)
          ("M-m g #" . eww-list-histories)
          ("M-m g {" . eww-back-url)
          ("M-m g }" . eww-forward-url))
  :config
  (progn
    (add-hook 'eww-mode-hook 'visual-line-mode)))

(use-package feature-mode
  :ensure t
  :diminish feature-mode
  :config
    (setq feature-step-search-path "features/**/*steps.rb")
    (setq feature-step-search-gems-path "gems/ruby/*/gems/*/**/*steps.rb")
    (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode)))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :defer 2
  :bind* (("M-m ] l"   . flycheck-next-error)
          ("M-m [ l"   . flycheck-previous-error)
          ("M-m SPC l" . flycheck-list-errors))
  :config
  (global-flycheck-mode))

(with-eval-after-load "which-key"
(which-key-add-key-based-replacements
  "] l"   "next error"
  "[ l"   "previous error"
  "SPC l" "list errors"))

(use-package flyspell
  :ensure t
  :diminish flyspell-mode
  :config
    (setq ispell-program-name "aspell")
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package git-timemachine
  :ensure t
  :commands (git-timemachine-toggle
             git-timemachine-switch-branch)
  :bind* (("M-m g l" . git-timemachine-toggle)
          ("M-m g L" . git-timemachine-switch-branch)))

(with-eval-after-load "which-key"
(which-key-add-key-based-replacements
  "g l" "git time machine"
  "g L" "time machine switch branch"))

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config
    (golden-ratio-mode 1)
    (add-to-list 'golden-ratio-extra-commands 'ace-window))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
    (defvar helm-buffer-max-length)
    (helm-mode 1)
  :bind
    (("M-x"     . undefined)
     ("M-x"     . helm-M-x)
     ("M-y"     . helm-show-kill-ring)
     ("C-x C-f" . helm-find-files)
     ("C-h b"   . helm-descbinds)
     ("C-x b"   . helm-mini)
     ("C-x C-b" . helm-mini)
     ("C-x C-d" . helm-browse-project)
     ("C-c h"   . helm-command-prefix)
     ("C-i"     . helm-execute-persistent-action)
     ("C-;"     . helm-flyspell-correct)
     ("C-z"     . helm-select-action)
  :map helm-map
    ("<tab>"    . helm-execute-persistent-action)
    ("C-i"      . helm-execute-persistent-action)
    ("C-z"      . helm-select-action))
  :config
    (setq helm-buffer-max-length 80)
    (helm-adaptive-mode t)
    (helm-autoresize-mode t)
    (helm-push-mark-mode t)
    (set-face-attribute 'helm-buffer-size nil      :foreground "knobColor" :background "#3F3F3F")
    (set-face-attribute 'helm-buffer-directory nil :foreground "knobColor" :background "#3F3F3F" :slant 'italic))

(use-package helm-flyspell
  :ensure t)

(use-package helm-swoop
  :ensure t
  :init
    (defvar helm-M-x-fuzzy-match)
    (defvar helm-recentf-fuzzy-match)
    (defvar helm-buffers-fuzzy-matching)
    (defvar helm-locate-fuzzy-match)
    (defvar helm-mode-fuzzy-match)
  :bind
  (("C-s"     . helm-swoop)
   ("M-i"     . helm-swoop)
   ("M-s s"   . helm-swoop)
   ("M-s M-s" . helm-swoop)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)
   :map isearch-mode-map
   ("M-i"     . helm-swoop-from-isearch)
   :map helm-swoop-map
   ("M-i"     . helm-multi-swoop-all-from-helm-swoop))
  :config
    (setq helm-swoop-split-with-multiple-windows nil)
    (setq helm-swoop-split-direction 'split-window-vertically)
    ;; Fuzzy matching for everything
    (setq helm-M-x-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-locate-fuzzy-match nil
        helm-mode-fuzzy-match t)
    (helm-autoresize-mode 1)
    (setq helm-autoresize-max-height 20
        helm-autoresize-min-height 20))

;; Make helm fuzzier
(use-package helm-fuzzier
  :ensure t
  :config
  (helm-fuzzier-mode 1))

;; to search in files
(use-package helm-swoop
  :ensure t
  :bind (("C-s" . helm-swoop-without-pre-input))
  :bind* (("M-m #"   . helm-swoop)
          ("M-m g /" . helm-multi-swoop)
          ("M-m o /" . helm-multi-swoop-org)
          ("M-m g E" . helm-multi-swoop-all))
  :init
  (setq helm-swoop-split-with-multiple-windows nil
        helm-swoop-split-direction 'split-window-vertically
        helm-swoop-split-window-function 'helm-default-display-buffer))

;; to help with projectile
(use-package helm-projectile
  :ensure t
  :bind*
    (("C-x p" . helm-projectile))
  :init
    (setq projectile-completion-system 'helm))

;; to describe bindings
(use-package helm-descbinds
  :ensure t
  :bind* (("M-m SPC ?" . helm-descbinds)))


(use-package helm-flycheck
  :ensure t
  :bind* (("M-m SPC l" . helm-flycheck)))

;; Flyspell errors with helm
(use-package helm-flyspell
  :ensure t)

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

(use-package js2-refactor
    :ensure t
    :diminish js2-refactor-mode
    :init
      (add-hook 'js2-mode-hook #'js2-refactor-mode)
    :config
      (js2r-add-keybindings-with-prefix "C-c r"))

(use-package json-mode
  :ensure t
  :diminish json-mode)

(use-package json-reformat
  :ensure t)

(use-package json-snatcher
  :ensure t
  :commands (jsons-print-path))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; generate table of contenst in markdown file
(use-package markdown-toc
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind*
    (("M-m ." . mc/edit-lines)
     ("M-m >" . mc/mark-next-like-this)
     ("M-m ," . mc/skip-to-next-like-this)
     ("M-m <" . mc/mark-previous-like-this)))

(use-package magit
  :ensure t
  :bind*
    (("M-m SPC e" . magit-status)
     ("M-m g b"   . magit-blame)))

(use-package neotree
  :ensure t
  :diminish neotree
  :config
    (setq neo-theme 'nerd))

(use-package origami
  :ensure t
  :commands (origami-toggle-node)
  :bind*
    (("M-m -" . orgiami-toggle-node)))

(use-package csharp-mode
  :ensure t
  :init (add-hook 'csharp-mode-hook 'omnisharp-mode)
  :bind
  (:map csharp-mode-map
        ("C-<return>". company-omnisharp)))

(use-package omnisharp
  :ensure t
  :diminish omnisharp-mode
  :config
    (setq omnisharp-server-executable-path "/usr/local/bin/omnisharp"))

(use-package projectile
  :ensure t
  :bind* (("M-m SPC d"   . projectile-find-file)
          ("M-m SPC D"   . projectile-switch-project)
          ("M-m SPC TAB" . projectile-find-other-file))
  :init
    (setq projectile-file-exists-remote-cache-expire (* 10 60))
  :diminish
    projectile-mode
  :config
    (projectile-mode))

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

(use-package restart-emacs
  :ensure t
  :bind* (("C-x M-c" . restart-emacs)))

(use-package ruby-mode
  :commands ruby-mode
  :mode (("Gemfile\\'" . ruby-mode)
         ("Kirkfile\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode)
         ("\\.builder\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("\\.irbrc\\'" . ruby-mode)
         ("\\.pryrc\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.rjs\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.rxml\\'" . ruby-mode))

  :init
  (setq ruby-use-encoding-map nil)

  :config
  (after-load 'ruby-mode
    (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
    (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command))

  ;; non-bundled ruby-mode isn't a derived mode of prog-mode
  ;; run these latter's hooks anyway in that case.
  (add-hook 'ruby-mode-hook
            (lambda ()
              (unless (derived-mode-p 'prog-mode)
                (run-hooks 'prog-mode-hook)))))

(use-package robe
  :ensure t
  :diminish robe-mode
  :config
    (add-to-list 'company-backends 'company-robe)
    (add-hook 'ruby-mode-hook 'robe-mode)
    (add-hook 'ruby-mode-hook (lambda () (robe-start))))

(use-package saveplace
  :init
    (save-place-mode 1)
  :config
    (if (fboundp #'save-place-mode)
      (save-place-mode +1)
    (setq-default save-place t))
    (setq save-place-limit nil)
    (defvar save-place-file)
    (setq save-place-file "~/.emacs.d/saveplace"))

(use-package spaceline
  :ensure t
  :demand t
  :config
    (set-face-attribute 'spaceline-highlight-face nil :foreground "#94BFF3")
    (set-face-attribute 'spaceline-highlight-face nil :background "#3F3F3F")
    (setq ns-use-srgb-colorspace nil) ;; fix colors on modeline
    (require 'spaceline-config)
    (defvar powerline-default-separator)
    (setq powerline-default-separator 'wave)
    (spaceline-spacemacs-theme)
    (spaceline-helm-mode))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  :config
  (sp-pair "<" ">" :wrap "C->")
  (smartparens-global-mode 1))

(use-package sql-indent
  :ensure t
  :pin melpa
  :config
  (eval-after-load "sql"
    '(load-library "sql-indent")))

(use-package rvm
  :ensure t
  :config
    (rvm-use-default))

(use-package tern
  :ensure t
  :diminish tern-mode
  :bind
    (("C-<return>" . company-tern))
  :defer 2
  :config
    (add-hook 'js-mode-hook (lambda () (tern-mode t))))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

(use-package volatile-highlights
  :ensure t
  :demand t
  :diminish volatile-highlights-mode
  :config
  (set-face-background 'vhl/default-face "#5b605e")
  (volatile-highlights-mode t))

(use-package web-beautify
  :ensure t
  :commands (web-beautify-css
             web-beautify-css-buffer
             web-beautify-html
             web-beautify-html-buffer
             web-beautify-js
             web-beautify-js-buffer))

(use-package web-mode
  :ensure t
  :mode
   ("\\.html$" . web-mode)
   ("\\.hbs$" . web-mode))

(use-package which-key
  :ensure t
  :defer t
  :diminish which-key-mode
  :init
    (setq which-key-sort-order 'which-key-key-order-alpha)
  :bind* (
    ("M-m ?" . which-key-show-top-level))
  :config
    (which-key-mode)
    (which-key-setup-side-window-right-bottom)
    (which-key-add-key-based-replacements "M-m ?" "top level bindings"))

(use-package yagist
  :ensure t
  :commands (yagist-region-or-buffer
             yagist-region-or-buffer-private)
  :bind* (("M-m g p" . yagist-region-or-buffer)
          ("M-m g P" . yagist-region-or-buffer-private))
  :init
  (setq yagist-encrypt-risky-config t))

(with-eval-after-load "which-key"
  (which-key-add-key-based-replacements
    "g p" "gist public"
    "g P" "gist private"))

(use-package yaml-mode
  :ensure t
  :diminish yamp-mode)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (setq yas-indent-line (quote none))
  :config
  (yas-reload-all)
  (yas-global-mode 1))

(use-package helm-c-yasnippet
  :ensure t
  :after yasnippet
  :init (bind-key "C-c y" #'helm-yas-complete)
  :config (validate-setq helm-yas-space-match-any-greedy t))

(provide 'setup-packages)

;;; setup-packages ends here
