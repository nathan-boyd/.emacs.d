;;; Package --- Summary Emacs Init File
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define clear in eshell ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define open in explorer function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun open-in-explorer ()
  (interactive)
  (cond
   ;; In buffers with file name
   ((buffer-file-name)
    (shell-command (concat "start explorer /e,/select,\""
                           (replace-regexp-in-string "/" "\\\\" (buffer-file-name)) "\"")))

   ;; In dired mode
   ((eq major-mode 'dired-mode)
    (shell-command (concat "start explorer /e,\""
                           (replace-regexp-in-string "/" "\\\\" (dired-current-directory)) "\"")))

   ;; In eshell mode
   ((eq major-mode 'eshell-mode)
    (shell-command (concat "start explorer /e,\"" (replace-regexp-in-string "/" "\\\\" (eshell/pwd)) "\"")))

   ;; Use default-directory as last resource
   (t
    (shell-command (concat "start explorer /e,\"" (replace-regexp-in-string "/" "\\\\" default-directory) "\"")))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define duplicate-line function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create shorcut for duplicate line ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-d") 'duplicate-line)

(provide 'my-modes)

;;; my-modes.el ends here
