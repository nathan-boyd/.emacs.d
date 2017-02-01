;;; beacon-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "beacon" "../../../../../.emacs.d/elpa/beacon-20161004.756/beacon.el"
;;;;;;  "a6d9e9e96205c419a499789d11c9d92b")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/beacon-20161004.756/beacon.el

(autoload 'beacon-blink "beacon" "\
Blink the beacon at the position of the cursor.
Unlike `beacon-blink-automated', the beacon will blink
unconditionally (even if `beacon-mode' is disabled), and this can
be invoked as a user command or called from lisp code.

\(fn)" t nil)

(defvar beacon-mode nil "\
Non-nil if Beacon mode is enabled.
See the `beacon-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `beacon-mode'.")

(custom-autoload 'beacon-mode "beacon" nil)

(autoload 'beacon-mode "beacon" "\
Toggle Beacon mode on or off.
With a prefix argument ARG, enable Beacon mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{beacon-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/beacon-20161004.756/beacon-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/beacon-20161004.756/beacon.el")
;;;;;;  (22673 16125 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; beacon-autoloads.el ends here
