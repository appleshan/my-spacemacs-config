;;; config.el --- appleshan-shell layer configuration file for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

;; Setup up Shell Environment
;; @see http://ergoemacs.org/emacs/emacs_env_var_paths.html

;; for Windows 10
(when (spacemacs/system-is-mswindows)
  (let ((mypaths
          '(
            "D:/bin"
            "C:/lang/python/Python361"
            "C:/lang/python/Python361/Scripts"
            ; "C:/lang/python/Python2713"
            ; "C:/lang/python/Python2713/Scripts"
            "D:/portable-soft/cmder"
            "D:/portable-soft/develop/PortableGit/bin"                      ;; git
            "D:/portable-soft/develop/PortableGit/mingw64/libexec/git-core" ;; git
            "D:/portable-soft/develop/diffutils-2.8.7-1/bin"                ;; diff
            "C:/Program Files (x86)/GnuPG/bin/"                             ;; gpg
            "C:/Windows/System32"
           )))
    (setq exec-path (append mypaths (list "." exec-directory)) )

    (setenv "PATH" (mapconcat 'identity mypaths ";") )
    (setenv "HOME" "D:/home/appleshan/" )
    (setenv "JAR_PATH" "D:/bin/java-lib" )
    ))

;; for linux
(when (spacemacs/system-is-linux)
  (let ((mypaths
          '(
            "/home/appleshan/bin"
            "/usr/local/sbin"
            "/usr/local/bin"
            "/usr/bin"
            "/usr/lib/jvm/default/bin"
            "/usr/bin/site_perl"
            "/usr/bin/vendor_perl"
            "/usr/bin/core_perl"
           )))
    (setq exec-path (append mypaths (list "." exec-directory)))

;    (setenv "PATH" (mapconcat 'identity mypaths ":") )

    (setenv "JAR_PATH" "/home/appleshan/bin/java-lib" )
    )

  ;(setenv "LD_LIBRARY_PATH" "/opt/oracle/lib")
  ;(setenv "NLS_LANG" "SIMPLIFIED CHINESE_CHINA.AL32UTF8")
  )

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; config.el ends here
