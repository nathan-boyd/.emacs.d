;;; Compiled snippets and support files for `snippets'
;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("package" "(use-package $0\n  :commands (isearch-moccur isearch-all)\n  :bind ((\"M-s O\" . moccur)\n         :map isearch-mode-map\n         (\"M-o\" . isearch-moccur))\n  :diminish $0\n  :init\n  (setq isearch-lazy-highlight t)\n  :config\n  (use-package moccur-edit))" "package" nil nil nil "/Users/nboyd/.emacs.d/snippets/emacs-lisp-mode/package" nil nil)))


;;; Do not edit! File generated at Fri Apr  7 19:03:04 2017
