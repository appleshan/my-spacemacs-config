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
(defun appleshan-javascript/beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

(defun appleshan-javascript/mdn-search (searchString)
  "Open a browser on the MDN page for SEARCHSTRING."
  (interactive (list (read-string "Search: " (thing-at-point 'symbol))))
  (browse-url (format "https://developer.mozilla.org/en-US/search?q=%s&topic=js" searchString)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
