;;; packages.el --- appleshan-complete layer packages file for Spacemacs

;; Copyright (c) 2016-2020 Apple Shan

;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  这个文件是 appleshan 个人专用的 emacs 配置文件，emacs 中文用户可以参考。

;;; License: GPLv3

;;; Code:

(setq appleshan-complete-packages
    '(
      ; helm
      ; ido
      ; ido-ubiquitous
      ; flx-ido
      counsel
      company
      ))

;; List of packages to exclude.
(setq appleshan-complete-excluded-packages '())

; (defun appleshan-complete/post-init-helm ()
;    (with-eval-after-load 'helm
;      ;; limit max number of matches displayed for speed
;      (setq helm-candidate-number-limit 100)
;      ;; ignore boring files like .o and .a
;      (setq helm-ff-skip-boring-files t)
;    ))

;; ido
; (defun appleshan-complete/post-init-ido ()
;   (use-package ido
;     :ensure nil
;     :config
;     (progn
;       (use-package org
;         :ensure nil
;         :config
;         (setq org-completion-use-ido t))

;       (setq ido-enable-regexp t
;             ido-enable-prefix nil
;             ido-create-new-buffer 'always
;             ido-file-extensions-order '(".org" ".py" ".el" ".java" ".js" ".el" ".xml")
;             ido-use-filename-at-point 'guess
;             ido-auto-merge-work-directories-length -1
;             ido-auto-merge-delay-time 2
;             ido-use-url-at-point t
;             ido-use-faces nil
;             gc-cons-threshold 20000000)

;       ;; ido sort
;       (add-hook 'ido-make-file-list-hook 'appleshan/ido-sort-mtime) ; 文件的排序方法
;       (add-hook 'ido-make-dir-list-hook 'appleshan/ido-sort-mtime)  ; 目录的排序方法

;       (defun appleshan/ido-sort-mtime ()
;         (setq ido-temp-list
;               (sort ido-temp-list
;                     (lambda (a b)
;                       (time-less-p
;                        (sixth (file-attributes (concat ido-current-directory b)))
;                        (sixth (file-attributes (concat ido-current-directory a)))))))
;         (ido-to-end  ;move . files to end (again)
;          (delq nil (mapcar
;                     (lambda (x) (and (char-equal (string-to-char x) ?.) x))
;                     ido-temp-list))))

;       ;; ido keybindings
;       (add-hook 'ido-setup-hook 'appleshan/ido-keybinding)
;       (defun appleshan/ido-keybinding ()
;         (define-key ido-completion-map (kbd "C-SPC") nil)
;         (define-key ido-completion-map (kbd "C-@") nil)
;         (define-key ido-completion-map (kbd "C-i") 'ido-edit-input)
;         (define-key ido-completion-map (kbd "C-l") 'ido-delete-backward-updir))
;       (global-set-key (kbd "C-x C-b") 'ido-display-buffer)
;     )))

; (defun appleshan-complete/init-ido-ubiquitous ()
;   (use-package ido-ubiquitous
;     :config
;     (setq ido-everywhere t)
;     (ido-ubiquitous-mode 1)))

; (defun appleshan-complete/post-init-flx-ido ()
;   (use-package flx-ido
;     :config
;     (setq flx-ido-use-faces t)))

;; swiper and ivy-mode

(defun appleshan-complete/post-init-counsel ()
  (use-package counsel
  	:config
  	(progn
  	  (setq spacemacs--counsel-commands
        '(("sift" . "sift --no-color -nr %s %S .")))
      )))

;; company-mode
(defun appleshan-complete/post-init-company ()
  (with-eval-after-load 'company
  	(setq company-selection-wrap-around t)
  	(setq company-show-numbers t)

    ;; company-dabbrev
    (setq company-dabbrev-char-regexp "[[:word:]_:@.-]+")
    (setq company-dabbrev-minimum-length 2)

    (global-set-key (kbd "M-/") 'company-complete)

    (when (configuration-layer/package-usedp 'company)
      (spacemacs|add-company-hook shell-script-mode)
      (spacemacs|add-company-hook nxml-mode)
      (spacemacs|add-company-hook conf-unix-mode)
      )
  ))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
