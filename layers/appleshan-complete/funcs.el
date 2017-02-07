;;; funcs.el --- appleshan-complete Layer functions File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar helm-httpstatus-source
  '((name . "HTTP STATUS")
    (candidates . (("100 Continue")
                   ("101 Switching Protocols")
                   ("102 Processing")
                   ("200 OK")
                   ("201 Created")
                   ("202 Accepted")
                   ("203 Non-Authoritative Information")
                   ("204 No Content")
                   ("205 Reset Content")
                   ("206 Partial Content")
                   ("207 Multi-Status")
                   ("208 Already Reported")
                   ("300 Multiple Choices")
                   ("301 Moved Permanently")
                   ("302 Found")
                   ("303 See Other")
                   ("304 Not Modified")
                   ("305 Use Proxy")
                   ("307 Temporary Redirect")
                   ("400 Bad Request")
                   ("401 Unauthorized")
                   ("402 Payment Required")
                   ("403 Forbidden")
                   ("404 Not Found")
                   ("405 Method Not Allowed")
                   ("406 Not Acceptable")
                   ("407 Proxy Authentication Required")
                   ("408 Request Timeout")
                   ("409 Conflict")
                   ("410 Gone")
                   ("411 Length Required")
                   ("412 Precondition Failed")
                   ("413 Request Entity Too Large")
                   ("414 Request-URI Too Large")
                   ("415 Unsupported Media Type")
                   ("416 Request Range Not Satisfiable")
                   ("417 Expectation Failed")
                   ("418 I'm a teapot")
                   ("421 Misdirected Request")
                   ("422 Unprocessable Entity")
                   ("423 Locked")
                   ("424 Failed Dependency")
                   ("425 No code")
                   ("426 Upgrade Required")
                   ("428 Precondition Required")
                   ("429 Too Many Requests")
                   ("431 Request Header Fields Too Large")
                   ("449 Retry with")
                   ("500 Internal Server Error")
                   ("501 Not Implemented")
                   ("502 Bad Gateway")
                   ("503 Service Unavailable")
                   ("504 Gateway Timeout")
                   ("505 HTTP Version Not Supported")
                   ("506 Variant Also Negotiates")
                   ("507 Insufficient Storage")
                   ("509 Bandwidth Limit Exceeded")
                   ("510 Not Extended")
                   ("511 Network Authentication Required")))
      (action . message)))

(defun helm-httpstatus ()
  (interactive)
  (helm-other-buffer '(helm-httpstatus-source) "*helm httpstatus*"))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
