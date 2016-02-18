;;; package --- Summary
;;; Commentary:
;;;     setup flycheck
;;; Code:

(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(global-flycheck-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'js-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)

(setq-default flycheck-temp-prefix ".flycheck")

(provide 'setup-flycheck)

;;; setup-flycheck ends here
