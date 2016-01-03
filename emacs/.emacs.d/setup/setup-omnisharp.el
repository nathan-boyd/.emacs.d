;;; package --- Summary 
;;; Commentary: 
;;; Code:

;; (add-hook 'csharp-mode-hook 'omnisharp-mode)
(setq omnisharp-server-executable-path "D:\\gitHubRepos\\omnisharp-server\\OmniSharp\\bin\\Debug\\omnisharp.exe")
(require 'omnisharp-utils)
(require 'omnisharp-server-actions)
(require 'omnisharp-auto-complete-actions)
(require 'omnisharp-settings)

(setq gc-cons-threshold 20000000)
(setq omnisharp-company-strip-trailing-brackets nil)

(defun my-csharp-mode ()
  (add-to-list 'company-backends 'company-omnisharp)
  (yas-minor-mode)
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode))

(add-hook 'csharp-mode-hook 'my-csharp-mode)

(defun omnisharp-unit-test (mode)
  "Run tests after building the solution. Mode should be one of 'single', 'fixture' or 'all'" 
  (interactive)
  (let ((test-response
         (omnisharp-post-message-curl-as-json
          (concat (omnisharp-get-host) "gettestcontext") 
          (cons `("Type" . ,mode) (omnisharp--get-common-params)))))
    (let ((test-command
           (cdr (assoc 'TestCommand test-response)))

          (test-directory
           (cdr (assoc 'Directory test-response))))
      (cd test-directory)
      (compile test-command))))

(defun csharp-newline-and-indent ()
  "Open a newline and indent. If point is between a pair of braces, opens newlines to put braces on their own line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (and
             (looking-at " *}")
             (save-match-data
               (when (looking-back "{ *")
                 (goto-char (match-beginning 0))
                 (unless (looking-back "^[[:space:]]*")
                   (newline-and-indent))
                 t)))
        (unless (and (boundp electric-pair-open-newline-between-pairs)
                     electric-pair-open-newline-between-pairs
                     electric-pair-mode)
          (goto-char (match-beginning 0))
          (newline-and-indent)))))
  (newline-and-indent)) 

(with-eval-after-load 'company
(define-key company-active-map (kbd ".") (lambda() (interactive) (company-complete-selection-insert-key-and-complete '".")))
(define-key company-active-map (kbd "]") (lambda() (interactive) (company-complete-selection-insert-key-and-complete '"]")))
(define-key company-active-map (kbd "[") (lambda() (interactive) (company-complete-selection-insert-key '"[")))
(define-key company-active-map (kbd ")") (lambda() (interactive) (company-complete-selection-insert-key '")")))
(define-key company-active-map (kbd "<SPC>") nil)
(define-key company-active-map (kbd ";") (lambda() (interactive) (company-complete-selection-insert-key '";")))
(define-key company-active-map (kbd ">") (lambda() (interactive) (company-complete-selection-insert-key '">")))
(define-key omnisharp-mode-map (kbd "}") 'csharp-indent-function-on-closing-brace) 
(define-key omnisharp-mode-map (kbd "<RET>") 'csharp-newline-and-indent))

(define-key omnisharp-mode-map (kbd "<f12>") 'omnisharp-go-to-definition)
(define-key omnisharp-mode-map (kbd "s-d") 'omnisharp-go-to-definition)
(define-key omnisharp-mode-map (kbd "S-s-<up>") 'omnisharp-navigate-up)
(define-key omnisharp-mode-map (kbd "S-s-<down>") 'omnisharp-navigate-down)
(define-key omnisharp-mode-map (kbd "S-<f12>") 'omnisharp-helm-find-usages)

(define-key omnisharp-mode-map (kbd "s-u") 'omnisharp-helm-find-usages)
(define-key omnisharp-mode-map (kbd "s-i") 'omnisharp-helm-find-implementations)
(define-key omnisharp-mode-map (kbd "S-s-<f12>") 'omnisharp-helm-find-usages)
(define-key omnisharp-mode-map (kbd "<M-RET>") 'omnisharp-run-code-action-refactoring)
(define-key omnisharp-mode-map (kbd "C-.") 'omnisharp-run-code-action-refactoring)

(define-key omnisharp-mode-map (kbd "C-k C-d") 'omnisharp-code-format)
(define-key omnisharp-mode-map (kbd "C-d") 'duplicate-current-line-or-region)

(define-key omnisharp-mode-map (kbd "<f2>") 'omnisharp-rename-interactively)
(define-key omnisharp-mode-map (kbd "<f5>") 'omnisharp-build-in-emacs)

;; disable emacs ctrl-r key.... we need it for VS shortcuts
(global-unset-key "\C-r")
(define-key omnisharp-mode-map (kbd "C-r C-t") (lambda() (interactive) (omnisharp-unit-test "single")))
(define-key omnisharp-mode-map (kbd "C-r C-a") (lambda() (interactive) (omnisharp-unit-test "all")))
(define-key omnisharp-mode-map (kbd "C-r C-l") 'recompile)
(define-key omnisharp-mode-map (kbd "C-r C-r") 'omnisharp-rename)

(define-key omnisharp-mode-map (kbd "<M-RET>") 'omnisharp-run-code-action-refactoring)
(define-key omnisharp-mode-map (kbd "<C-.>") 'omnisharp-run-code-action-refactoring)

;; disable emacs ctrl-k key.... we need it for VS shortcuts
(global-unset-key "\C-k")
(global-set-key (kbd "C-k C-c") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-k C-u") 'comment-or-uncomment-region-or-line)

(provide 'setup-omnisharp)

;;; setup-omnisharp ends here
