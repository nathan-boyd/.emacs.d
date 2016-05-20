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
(setq-default ac-js2-evaluate-calls t)

; add minor modes for js files
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'js2-refactor-mode)

; Let flycheck handle parse errors
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t)

; setup js2refactor ;;
(js2r-add-keybindings-with-prefix "C-r C-r")
(add-hook 'js2-mode-hook #'js2-refactor-mode)

; configure js-beautify
(eval-after-load 'js2-mode
  '(add-hook
    'js2-mode-hook
    (lambda ()
      (setq web-beautify-args '("-f" "-" "--config" "D:/git/nb-tools/styles/.jsbeautifyrc"))
      ;;  (add-hook 'before-save-hook 'web-beautify-js-buffer t t) ; add this back once function foo () style is added to js-beautify
      )))

;(add-hook 'js-mode-hook '(lambda () (company-mode)))

(provide 'setup-js2-mode)

;;; setup-js2-mode ends here
