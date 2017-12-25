;;; funcs.el --- appleshan-python Layer functions File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;(defun elpy-goto-definition-or-rgrep ()
;  "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
;  (interactive)
;  (ring-insert find-tag-marker-ring (point-marker))
;  (condition-case nil (elpy-goto-definition)
;      (error (elpy-rgrep-symbol
;                 (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))

;(define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition-or-rgrep)

(defun company-yasnippet-or-completion ()
  "Solve company yasnippet conflicts."
  (interactive)
  (let ((yas-fallback-behavior
         (apply 'company-complete-common nil)))
    (yas-expand)))

(add-hook 'company-mode-hook
          (lambda ()
            (substitute-key-definition
             'company-complete-common
             'company-yasnippet-or-completion
             company-active-map)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
