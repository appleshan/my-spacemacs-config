;;; funcs.el --- appleshan-javascript Layer functions File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; reformat your json file, it requires python
(defun appleshan/beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

(defun appleshan/mdn-search (searchString)
  "Open a browser on the MDN page for SEARCHSTRING."
  (interactive (list (read-string "Search: " (thing-at-point 'symbol))))
  (browse-url (format "https://developer.mozilla.org/en-US/search?q=%s&topic=js" searchString)))

(defun my-js2-mode-hook ()
  (progn
    (define-key js2-mode-map "\C-ci" 'my-js-doc-insert-function-doc-snippet)
    (define-key js2-mode-map "@" 'js-doc-insert-tag)
    (modify-syntax-entry ?_ "w")
    (which-function-mode t)
    (setq imenu-create-index-function 'js2-imenu-make-index)

    (setq mode-name "JS2")
    (define-key js2-mode-map   (kbd "s-.") 'company-tern)
    (spacemacs/toggle-syntax-checking-on)
    (setq forward-sexp-function nil)
    (set (make-local-variable 'semantic-mode) nil)
    ))

;; http://blog.binchen.org/posts/use-which-func-mode-with-js2-mode.html
(defun my-which-function ()
  ;; clean the imenu cache
  ;; @see http://stackoverflow.com/questions/13426564/how-to-force-a-rescan-in-imenu-by-a-function
  (setq imenu--index-alist nil)
  (which-function))

(defun my-toggle-web-indent ()
  (interactive)
  ;; web development
  (if (eq major-mode 'json-mode)
      (progn
        (setq js-indent-level (if (= js-indent-level 2) 4 2))))

  (if (or (eq major-mode 'js-mode) (eq major-mode 'js2-mode))
      (progn
        (setq js-indent-level (if (= js-indent-level 2) 4 2))))

  (if (eq major-mode 'web-mode)
      (progn (setq web-mode-markup-indent-offset (if (= web-mode-markup-indent-offset 2) 4 2))
             (setq web-mode-css-indent-offset (if (= web-mode-css-indent-offset 2) 4 2))
             (setq web-mode-code-indent-offset (if (= web-mode-code-indent-offset 2) 4 2))))
  (if (eq major-mode 'css-mode)
      (setq css-indent-offset (if (= css-indent-offset 2) 4 2)))

  (setq indent-tabs-mode nil))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
