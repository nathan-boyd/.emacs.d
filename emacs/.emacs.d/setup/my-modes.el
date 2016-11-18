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

;;;;;;;;;;;;;;;;;
;; clear shell ;;
;;;;;;;;;;;;;;;;;
(defun clear-shell ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

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

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile emacs lisp setup files ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun remove-elc-on-save () 
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

(defun comment-batch-seperators ()
  "comment out GO, for edbi editing"
  (interactive)
  (beginning-of-buffer)
  (while (re-search-forward "\nGO" nil t)
    (replace-match "\n-- GO"))
  )

(defun uncomment-batch-seperators ()
  "un-comment GO, for edbi editing"
  (interactive)
  (beginning-of-buffer)
  (while (re-search-forward "\n-- GO" nil t)
    (replace-match "\nGO"))
  )

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(provide 'my-modes)

;;; my-modes.el ends here
