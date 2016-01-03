;;; package 
;;; Commentary: setup emacs
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;
;; setup web-beautify ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; (eval-after-load 'js2-mode
    ;; (eval-after-load 'js2-mode
    ;;   '(add-hook 'js2-mode-hook
    ;;              (lambda ()
    ;;                (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

    ;; (eval-after-load 'json-mode
    ;;   '(add-hook 'json-mode-hook
    ;;              (lambda ()
    ;;                (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

    (eval-after-load 'css-mode
      '(add-hook 'css-mode-hook
                 (lambda ()
                   (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

(provide 'setup-web-beautify)
;;; setup-web-beautify.el ends here
