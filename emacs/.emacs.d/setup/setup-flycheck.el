;;; package --- Summary js2-mode setup file

;;; Commentary: setup emacs

;;; Code:

;;;;;;;;;;;;;;;;;;;;
;; setup flycheck ;;
;;;;;;;;;;;;;;;;;;;;

(flycheck-def-config-file-var flycheck-jscs javascript-jscs "~/.jscsrc" :safe #'stringp)
(flycheck-define-checker javascript-jscs "A JavaScript code style checker. See URL `https://github.com/mdevils/node-jscs'."
  :command ("jscs" "--reporter" "checkstyle" 
            (config-file "--config" flycheck-jscs)
            source)
  :error-parser flycheck-parse-checkstyle
  :modes (js-mode js2-mode)
  :next-checkers (javascript-jshint))

(add-to-list 'flycheck-checkers 'javascript-jscs)
