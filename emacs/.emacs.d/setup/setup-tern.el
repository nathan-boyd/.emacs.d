;;; package --- Summary
;;; Commentary:
;;;     setup package
;;; Code:

(add-to-list 'load-path "D:/apps/nvm/v4.2.6/node_modules/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'web-mode-hook (lambda () (tern-mode t)))

(provide 'setup-tern)

;;; setup-tern ends here
