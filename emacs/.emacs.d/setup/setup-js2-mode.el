;;; package --- Summary
;;; Commentary:
;;;     setup js2-mode
;;; Code:

; setup js2 major mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default indent-tabs-mode nil)
(setq-default js2-idle-timer-delay 0.1)
(setq-default js2-indent-on-enter-key nil)
(setq-default js2-enter-indents-newline nil)
(setq-default js2-highlight-level 3)
(setq-default js2-basic-offset 2)

; add minor modes for js files
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'js2-refactor-mode)

; Let flycheck handle parse errors
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t)

(provide 'setup-js2-mode)

;;; setup-js2-mode ends here
