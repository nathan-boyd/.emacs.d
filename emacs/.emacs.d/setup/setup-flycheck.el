;;; package --- Summary
;;; Commentary:
;;;     setup flycheck
;;; Code:

(defun flycheck-error-sexp-region (err)
  "Get the sexp region of ERR.

ERR is a Flycheck error whose region to get.

Return a cons cell `(BEG . END)' where BEG is the beginning of
the symbol at the error column, and END the end of the symbol.
If ERR has no error column, or if there is no symbol at this
column, return nil."
  (-when-let (column (car (flycheck-error-column-region err)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char column)
        (condition-case ()
            (forward-sexp 1)
          (error (forward-symbol 1)))
        (cons column (point))))))

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
(flycheck-add-mode 'javascript-standard 'js2-mode)
(flycheck-add-mode 'javascript-standard 'js-mode)
(flycheck-add-mode 'javascript-standard 'web-mode)

(setq-default flycheck-temp-prefix ".flycheck")

(provide 'setup-flycheck)

;;; setup-flycheck ends here
