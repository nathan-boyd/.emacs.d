;;; package --- Summary
;;; Commentary:
;;;     setup package
;;; Code:

(add-to-list 'load-path "C:/Users/nboyd/AppData/Roaming/npm/node_modules/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(provide 'setup-tern)

;;; setup-tern ends here
