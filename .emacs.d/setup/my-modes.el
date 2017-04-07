;;; Package --- Summary Emacs Init File
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;
;; clear shell ;;
;;;;;;;;;;;;;;;;;
(defun my/clear-shell ()
  "Clear text in shell buffers."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define duplicate-line function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/duplicate-line()
  "Duplicate current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

;; bind key and override all minor modes with *
(bind-keys*
 ("M-m d" . my/duplicate-line))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comment out sql batch seperator "go" ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun comment-batch-seperators ()
  "Comment out GO, for edbi editing."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\nGO" nil t)
    (replace-match "\n-- GO")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uncomment sql batch seperator "go" ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uncomment-batch-seperators ()
  "Un-comment GO, for edbi editing."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\n-- GO" nil t)
    (replace-match "\nGO")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions to align text by character ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/align-whitespace (start end)
  "Align columns by whitespace.  START END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun my/align-ampersand (start end)
  "Align columns by ampersand.   START END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))

(defun my/align-quote-space (start end)
  "Align columns by quote and space.  START END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\).*\\s-\"" 1 0 t))

(defun my/align-equals (start end)
  "Align columns by equals sign.  START END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)=" 1 0 t))

(defun my/align-comma (start end)
  "Align columns by comma.  START END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)," 1 1 t))

(defun my/align-dot (start end)
  "Align columns by dot.  START END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\\." 1 1 t))

(defun my/align-colon (start end)
  "Align columns by equals sign.  START END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\):" 1 0 t))

(bind-keys*
 ("M-m g A SPC" . my/align-whitespace)
 ("M-m g A &"   . my/align-ampersand)
 ("M-m g A ,"   . my/align-comma)
 ("M-m g A \""  . my/align-quote-space)
 ("M-m g A ."   . my/align-dot)
 ("M-m g A ="   . my/align-equals)
 ("M-m g A :"   . my/align-colon)
 ("M-m g A A"   . align-regexp))

(which-key-add-key-based-replacements
  "g A SPC" "align based on spaces"
  "g A &"   "align based on &"
  "g A ,"   "align based on ,"
  "g A \""  "align based on \""
  "g A ."   "align based on ."
  "g A ="   "align based on ="
  "g A :"   "align based on :"
  "g A A"   "align based on regex")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to insert date ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/insert-date (prefix)
  "Insert the current date.  With prefix-argument, write out the day and month name.  PREFIX."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%A, %d %B %Y")
                 ((equal prefix '(16)) "%Y-%m-%d %H:%M:%S"))))
    (insert (format-time-string format))))

(bind-keys*
 ("M-m g D" . my/insert-date))

(which-key-add-key-based-replacements
  "g D"   "insert date")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rename file and buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(bind-keys*
 ("M-m g R" . my/rename-current-buffer-file))

(which-key-add-key-based-replacements
  "g R" "rename buffer and file")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete current buffer and file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(bind-keys*
 ("M-m g K" . my/delete-current-buffer-file))

(which-key-add-key-based-replacements
  "g K" "delete buffer and file")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; copy current file name to buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/copy-current-file-path ()
  "Add current file path to kill ring.  Limits the filename to project root if possible."
  (interactive)
  (kill-new buffer-file-name))

(bind-keys*
 ("M-m g y" . my/copy-current-file-path))

(which-key-add-key-based-replacements
  "g y" "copy current file path")

;;;;;;;;;;;;;;;
;; copy line ;;
;;;;;;;;;;;;;;;
(defun my/copy-to-end-of-line ()
  "Copy from cursor to end of line."
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))

(bind-keys*
 ("M-m Y" . my/copy-to-end-of-line))

(which-key-add-key-based-replacements
  "Y" "copy till end of line")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom keybindings without functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-keys*
 ("C--" . pop-tag-mark))

(provide 'my-modes)

;;; my-modes.el ends here
