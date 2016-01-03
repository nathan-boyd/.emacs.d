;;; package --- Summary
;;; Commentary: setup emacs
;;; Code:

; disable tabs globally
(electric-indent-mode -1)
(setq-default indent-tabs-mode nil)

(smart-tabs-add-language-support csharp csharp-mode-hook
  ((c-indent-line . c-basic-offset)
   (c-indent-region . c-basic-offset)))

(smart-tabs-insinuate 'csharp 'javascript)

(setq-default tab-width 4)
(setq cua-auto-tabify-rectangles nil)

(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode indent-tabs-mode))
    (if (memq indent-line-function
              '(indent-relative
                indent-relative-maybe))
        (setq indent-tabs-mode nil))
        ad-do-it))

(defmacro smart-tabs-advice (function offset)
  `(progn
     (defvaralias ',offset 'tab-width)
     (defadvice ,function (around smart-tabs activate)
       (cond
        (indent-tabs-mode
        (save-excursion
          (beginning-of-line)
          (while (looking-at "\t*\\( +\\)\t+")
            (replace-match "" nil nil nil 1)))
        (setq tab-width tab-width)
        (let ((tab-width fill-column)
              (,offset fill-column)
              (wstart (window-start)))
          (unwind-protect
              (progn ad-do-it)
            (set-window-start (selected-window) wstart))))
        (t
         ad-do-it)))))

(smart-tabs-advice python-indent-line-1 python-indent)
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width (default-value 'tab-width))))

(smart-tabs-advice js-indent-line-1 js-indent)
(add-hook 'js-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width (default-value 'tab-width))))

(smart-tabs-advice c-indent-line c-basic-offset)
(smart-tabs-advice c-indent-region c-basic-offset)

;; to "make it work" for js
;; (setq-default indent-tabs-mode 1)
;; (setq-default tab-width 4)
;; (setq indent-line-function 'insert-tab)

(provide 'setup-tabs)

;;; setup-tabs ends here
