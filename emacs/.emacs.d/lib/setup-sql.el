;;; Package --- Summary Emacs Init File
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup sql integration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar comint-buffer-maximum-size)
(defvar comint-scroll-show-maximum-output)
(defvar comint-input-ring-size)
(add-hook 'sql-interactive-mode-hook (function (lambda ()
    (toggle-truncate-lines t)
    (setq comint-output-filter-functions 'comint-truncate-buffer
          comint-buffer-maximum-size 5000
          comint-scroll-show-maximum-output t
          comint-input-ring-size 500))))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(defvar sql-connection-alist)
(setq sql-connection-alist
    '(
      (appdb 
            (sql-product 'ms)
            (sql-server "localhost")
            (sql-database "appdb")
            (sql-user "")
            (sql-password ""))
        (apilog 
            (sql-product 'ms)
            (sql-server "localhost")
            (sql-database "apilog")
            (sql-user "")
            (sql-password ""))
	(configdb_1
            (sql-product 'ms)
            (sql-server "localhost\\db1")
            (sql-database "configdb")
            (sql-user "")
            (sql-password ""))
	(configdb_2
            (sql-product 'ms)
            (sql-server "localhost\\db2")
            (sql-database "configdb")
            (sql-user "")
            (sql-password ""))
))

(defvar sql-product)
(defun nb-sql (connection)
    "Connect to the input server using sql-connection-alist"
    (interactive
    (helm-comp-read "Select server: " (mapcar (lambda (item)
    (list
    (symbol-name (nth 0 item))
    (nth 0 item)))
    sql-connection-alist)))
    (let*((connection-info (assoc connection sql-connection-alist))
            (connection-product (nth 1 (nth 1 (assoc 'sql-product connection-info)))))
        (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
        (add-to-list 'sql-connection-alist connection-info)
        (setq sql-product connection-product)

        (if current-prefix-arg
            (sql-connect connection connection)
            (sql-connect connection)
        )
    )
)

(provide 'setup-sql)

;;; setup-sql ends here
