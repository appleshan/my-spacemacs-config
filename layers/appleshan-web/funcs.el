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

(defun appleshan/web-mode-setup ()
  ;; indentation
  ;; HTML offset indentation
  (setq web-mode-markup-indent-offset 2)
  ;; CSS offset indentation
  (setq web-mode-css-indent-offset 2)
  ;; Script offset indentation (for JavaScript, Java, PHP, etc.)
  (setq web-mode-code-indent-offset 2)
  ;; HTML content indentation
  (setq web-mode-indent-style 2)

  ;; @see https://emacs-china.org/t/web-mode/2130
  ;; padding
  ;; For <style> parts
  (setq web-mode-style-padding 0)
  ;; For <script> parts
  (setq web-mode-script-padding 0)
  ;; For multi-line blocks
  (setq web-mode-block-padding 0)

  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  )

(defun appleshan/impatient-mode-hook ()
  "my web mode hook for HTML REPL"
  (interactive)
  (impatient-mode)
  (spacemacs|hide-lighter impatient-mode)
  (httpd-start))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
