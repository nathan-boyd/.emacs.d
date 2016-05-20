;;; package --- Summary
;;; Commentary:
;;;     setup flycheck
;;; Code:

(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)
    '(javascript-eslint)
    '(javascript-gjslint)
    '(javascript-jscs)))

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(global-flycheck-mode)
;; (flycheck-add-mode 'javascript-standard 'js2-mode) 
(flycheck-add-mode 'javascript-standard 'js-mode)
(flycheck-add-mode 'javascript-standard 'web-mode)

(setq-default flycheck-temp-prefix ".flycheck")

(provide 'setup-flycheck)

;;; setup-flycheck ends here
