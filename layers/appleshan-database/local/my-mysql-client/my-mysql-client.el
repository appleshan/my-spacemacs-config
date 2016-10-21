;;; my-mysql-client.el --- Setup Emacs as an SQL Database client
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; default login params
;; (setq sql-postgres-login-params
;;       '((user :default "postgres")
;;         (database :default "postgres")
;;         (server :default "localhost")
;;         (port :default 5432)))

;;; hooks
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)
            (setq-local show-trailing-whitespace nil)
            ;; (auto-complete-mode t) ;; TODO
            ))

(add-hook 'sql-mode-hook
          (lambda ()
            (setq-local ac-ignore-case t)
            (auto-complete-mode)))

;;; server list
(setq sql-connection-alist
      '((mysql.nfdev.local (sql-product 'mysql)
                           (sql-port 3306)
                           (sql-server "192.168.14.41")
                           (sql-user "nfdev")
                           (sql-database "nfdev"))
        (mysql.nftest.local (sql-product 'mysql)
                            (sql-port 3306)
                            (sql-server "192.168.14.60")
                            (sql-user "nftest")
                            (sql-database "nftest"))))

;;; TODO update this function
(defun appleshan/sql-connect-server (connection)
  "Connect to the input server using sql-connection-alist"
  (interactive
   (helm-comp-read "Select server: " (mapcar (lambda (item)
                                               (list
                                                (symbol-name (nth 0 item))
                                                (nth 0 item)))
                                             sql-connection-alist)))
  ;; password
  (require 'my-password "~/Dropbox/emacs/my-spacemacs-config.d/layers/appleshan-database/local/my-mysql-client/my-password.el.gpg")
  ;; get the sql connection info and product from the sql-connection-alist
  (let* ((connection-info (assoc connection sql-connection-alist))
         (connection-product (nth 1 (nth 1 (assoc 'sql-product connection-info))))
         (sql-password (nth 1 (assoc connection my-sql-password))))
    (message "connection-info = %s" connection-info)
    (message "connection-product = %s" connection-product)
    (message "sql-password = %s" sql-password)
    ;; delete the connection info from the sql-connection-alist
    (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
    ;; delete the old password from the connection-info
    (setq connection-info (assq-delete-all 'sql-password connection-info))
    ;; add the password to the connection-info
    (nconc connection-info `((sql-password ,sql-password)))
    ;; add back the connection info to the beginning of sql-connection-alist
    ;; (last used server will appear first for the next prompt)
    (add-to-list 'sql-connection-alist connection-info)
    ;; override the sql-product by the product of this connection
    (setq sql-product connection-product)
    ;; connect
    (if current-prefix-arg
        (sql-connect connection connection)
      (sql-connect connection))))


(provide 'my-mysql-client)

;;; my-mysql-client.el ends here
