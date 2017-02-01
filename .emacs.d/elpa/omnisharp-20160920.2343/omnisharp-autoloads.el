;;; omnisharp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "omnisharp" "../../../../../.emacs.d/elpa/omnisharp-20160920.2343/omnisharp.el"
;;;;;;  "878ba12e5e9d31421162a6ce19e59b75")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/omnisharp-20160920.2343/omnisharp.el

(autoload 'omnisharp-mode "omnisharp" "\
Omnicompletion (intellisense) and more for C# using an OmniSharp
server backend.

\(fn &optional ARG)" t nil)

(autoload 'omnisharp-fix-code-issue-at-point "omnisharp" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "omnisharp-auto-complete-actions" "../../../../../.emacs.d/elpa/omnisharp-20160920.2343/omnisharp-auto-complete-actions.el"
;;;;;;  "aa32399890c71c7433d152b69e9e53b3")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/omnisharp-20160920.2343/omnisharp-auto-complete-actions.el

(autoload 'company-omnisharp "omnisharp-auto-complete-actions" "\


\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "omnisharp-server-actions" "../../../../../.emacs.d/elpa/omnisharp-20160920.2343/omnisharp-server-actions.el"
;;;;;;  "9bac7802b903a81e13ad448b8fba3fdb")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/omnisharp-20160920.2343/omnisharp-server-actions.el

(autoload 'omnisharp-start-omnisharp-server "omnisharp-server-actions" "\
Starts an OmniSharpServer for a given path to a solution file or a directory

\(fn PATH-TO-SOLUTION)" t nil)

(autoload 'omnisharp-check-alive-status "omnisharp-server-actions" "\
Shows a message to the user describing whether the
OmniSharpServer process specified in the current configuration is
alive.
\"Alive\" means it is running and not stuck. It also means the connection
to the server is functional - I.e. The user has the correct host and
port specified.

\(fn)" t nil)

(autoload 'omnisharp-check-ready-status "omnisharp-server-actions" "\
Shows a message to the user describing whether the
OmniSharpServer process specified in the current configuration has
finished loading the solution.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/omnisharp-20160920.2343/example-config-for-evil-mode.el"
;;;;;;  "../../../../../.emacs.d/elpa/omnisharp-20160920.2343/omnisharp-auto-complete-actions.el"
;;;;;;  "../../../../../.emacs.d/elpa/omnisharp-20160920.2343/omnisharp-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/omnisharp-20160920.2343/omnisharp-pkg.el"
;;;;;;  "../../../../../.emacs.d/elpa/omnisharp-20160920.2343/omnisharp-server-actions.el"
;;;;;;  "../../../../../.emacs.d/elpa/omnisharp-20160920.2343/omnisharp-settings.el"
;;;;;;  "../../../../../.emacs.d/elpa/omnisharp-20160920.2343/omnisharp-utils.el"
;;;;;;  "../../../../../.emacs.d/elpa/omnisharp-20160920.2343/omnisharp.el")
;;;;;;  (22673 16161 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; omnisharp-autoloads.el ends here
