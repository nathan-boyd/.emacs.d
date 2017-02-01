;;; restart-emacs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "restart-emacs" "../../../../../.emacs.d/elpa/restart-emacs-20161108.2239/restart-emacs.el"
;;;;;;  "6875822d8a25ff6d17ab73a5a553d5e5")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/restart-emacs-20161108.2239/restart-emacs.el

(autoload 'restart-emacs "restart-emacs" "\
Restart Emacs.

When called interactively ARGS is interpreted as follows

- with a single `universal-argument' (`C-u') Emacs is restarted
  with `--debug-init' flag
- with two `universal-argument' (`C-u') Emacs is restarted with
  `-Q' flag
- with three `universal-argument' (`C-u') the user prompted for
  the arguments

When called non-interactively ARGS should be a list of arguments
with which Emacs should be restarted.

\(fn &optional ARGS)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/restart-emacs-20161108.2239/restart-emacs-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/restart-emacs-20161108.2239/restart-emacs.el")
;;;;;;  (22673 16162 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; restart-emacs-autoloads.el ends here
