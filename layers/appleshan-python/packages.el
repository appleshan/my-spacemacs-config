;;; packages.el --- appleshan-python Layer packages File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq appleshan-python-packages
    '(
      company
      (lsp-python :location (recipe
                           :fetcher github
                           :repo "emacs-lsp/lsp-python"))
      py-autopep8
      ))

;; List of packages to exclude.
(setq appleshan-python-excluded-packages '())

(defun appleshan-python/post-init-company ()
  (spacemacs|add-company-backends
    :backends (company-files company-capf)
    :modes python-mode
    :variables
    company-minimum-prefix-length 0
    company-idle-delay 0.5))

;; Python support for lsp-mode using pyls.
;; Install:
;; sudo pip install python-language-server
(defun appleshan-python/init-lsp-python ()
  (use-package lsp-python
    :commands lsp-python-enable
    :init (add-hook 'python-mode-hook #'lsp-python-enable)))

;; Autopep8
;; enable autopep8 formatting on save
;; ignoring:
;; - E501 - Try to make lines fit within --max-line-length characters.
;; - W293 - Remove trailing whitespace on blank line.
;; - W391 - Remove trailing blank lines.
;; - W690 - Fix various deprecated code (via lib2to3).
(defun appleshan-python/init-py-autopep8 ()
  (use-package py-autopep8
    :defer t
    :config
    (progn
      (setq py-autopep8-options '("--ignore=E501,W293,W391,W690"))
      (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
    )))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
