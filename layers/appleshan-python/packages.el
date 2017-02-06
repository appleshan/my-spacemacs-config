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
      elpy ; Emacs Lisp Python Environment
      py-autopep8
      ))

;; List of packages to exclude.
(setq appleshan-python-excluded-packages '())

;; require :
; pip install rope        # a python refactoring library
; pip install jedi        # Python自动补全库
; pip install flake8      # flake8 用来检查语法错误
; pip install importmagic # importmagic 用来自动引入需要的包
; pip install autopep8    # autopep8 用来检查PEP8规范
; pip install yapf        # yapf 用来格式化代码
(defun appleshan-python/init-elpy ()
  (use-package elpy
    :defer t
    :init (with-eval-after-load 'python (elpy-enable))
    :config
    (progn
      (setq elpy-modules '(elpy-module-sane-defaults
                           elpy-module-eldoc
                           elpy-module-highlight-indentation
                           elpy-module-pyvenv))

      (when (configuration-layer/layer-usedp 'auto-completion)
        (add-to-list 'elpy-modules 'elpy-module-company)
        (add-to-list 'elpy-modules 'elpy-module-yasnippet))

      (setq elpy-rpc-python-command "python3")

      ;; Configuring the backend
      (setq elpy-rpc-backend "jedi")
      (when (executable-find "ipython3")
        (elpy-use-ipython))

      (spacemacs|hide-lighter elpy-mode))
    :bind ("RET" . newline-and-indent)))

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
      (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
    )))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
