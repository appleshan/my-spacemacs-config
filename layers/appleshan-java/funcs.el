;;; funcs.el --- appleshan-lisp Layer functions File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; {{ maven
;; mvn compile
(defun mvn-compile-full ()
  (interactive)
  (mvn "dependency:sources"))

;; mvn package
(defun mvn-package ()
  (interactive)
  (mvn "package"))

;; mvn install
(defun mvn-install ()
  (interactive)
  (mvn "install"))
;; }}

(defun eclim/file-locate-ivy ()
  (interactive)
  (eclim/with-results hits ("locate_file" ("-p" "^.*$") ("-s" "workspace"))
    (find-file
      (ivy-completing-read "Select file:"
        (mapcar (lambda (hit) (assoc-default 'path hit)) hits)))))

;; {{ show documentation
(defun eclim//show-postip (doc)
  ;; TODO(jaigupta): Check if we can make use of the links in docs.
  (pos-tip-show (assoc-default 'text doc)))

;; Show a small java doc at point without opening a new buffer.
(defun eclim/java-show-documentation-for-current-element-postip ()
  (interactive)
    (let ((symbol (symbol-at-point)))
      (if symbol
          (let ((bounds (bounds-of-thing-at-point 'symbol))
                (window-config (current-window-configuration)))
            (eclim/with-results doc ("java_element_doc"
                                     ("-p" (eclim--project-name))
                                     "-f"
                                     ("-l" (- (cdr bounds) (car bounds)))
                                     ("-o" (save-excursion
                                             (goto-char (car bounds))
                                             (eclim--byte-offset))))
              (eclim//show-postip doc)))

        (message "No element found at point."))))
;; }}

; java run Main()
(defun java-run-standalone ()
  "Running small standalone programs."
  (interactive)
  (save-some-buffers 1)
  (compile (concat "javac_java.sh " (buffer-file-name (current-buffer)))))

(defun get-trace (segfault)
  "Following exceptions and spontaneous backtraces paths in Emacs"
  (interactive)
  (compile (concat "show-trace-in-emacs.pl" (if segfault " --segfault"))))

(defmacro ilambda (&rest body) `(lambda () (interactive) ,@body))

(add-hook 'java-mode-hook 
  '(lambda ()
     (local-set-key [(control return)] (ilambda (javacomp-standalone)))
     (local-set-key "\M-=" (ilambda (get-trace nil)))
     (local-set-key "\M--" (ilambda (get-trace t)))
         ))

(setq completion-ignored-extensions (cons ".class" completion-ignored-extensions))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
