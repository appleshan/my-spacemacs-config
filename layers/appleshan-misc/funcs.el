;;; funcs.el --- appleshan-misc Layer functions File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(defmacro adjust-major-mode-keymap-with-evil (m &optional r)
  `(eval-after-load (quote ,(if r r m))
     '(progn
        (evil-make-overriding-map ,(intern (concat m "-mode-map")) 'normal)
        ;; force update evil keymaps after git-timemachine-mode loaded
        (add-hook (quote ,(intern (concat m "-mode-hook"))) #'evil-normalize-keymaps))))

;; {{ scroll functions
(defun appleshan/hold-line-scroll-up()
  "Scroll the page with the cursor in the same line"
  (interactive)
  ;; move the cursor also
  (let ((tmp (current-column)))
    (scroll-up 1)
    (line-move-to-column tmp)
    (forward-line 1)))

(defun appleshan/hold-line-scroll-down()
  "Scroll the page with the cursor in the same line"
  (interactive)
  ;; move the cursor also
  (let ((tmp (current-column)))
    (scroll-down 1)
    (line-move-to-column tmp)
    (forward-line -1)))
;; }}

;; {{ Move Current Line Up or Down
;; @see http://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun appleshan/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun appleshan/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
;; }}

;; {{ insert date and time
(defun appleshan/now ()
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun appleshan/today ()
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d")))
;; }}

(defun appleshan/open-readme-in-git-root-directory ()
  (interactive)
  (let (filename
        (root-dir
          (locate-dominating-file
            (file-name-as-directory
              (file-name-directory buffer-file-name)) ".git"))
        )
    ;; (message "root-dir=%s" root-dir)
    (and root-dir (file-name-as-directory root-dir))
    (setq filename (concat root-dir "README.org"))
    (if (not (file-exists-p filename))
        (setq filename (concat root-dir "README.md"))
      )
    ;; (message "filename=%s" filename)
    (if (file-exists-p filename)
        (switch-to-buffer (find-file-noselect filename nil nil))
      (message "NO README.org or README.md found!"))
    ))

(add-hook 'minibuffer-inactive-mode-hook
          '(lambda() (set (make-local-variable 'semantic-mode) nil)))

(defun ascii-table ()
    "Display basic ASCII table (0 thru 128)."
    (interactive)
   (switch-to-buffer "*ASCII*")
    (erase-buffer)
    (setq buffer-read-only nil)        ;; Not need to edit the content, just read mode (added)
    (local-set-key "q" 'bury-buffer)   ;; Nice to have the option to bury the buffer (added)
    (setq lower32 '("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel"
        "bs" "ht" "nl" "vt" "np" "cr" "so" "si"
        "dle" "dc1" "dc2" "dc3" "dc4" "nak" "syn" "etb"
        "can" "em" "sub" "esc" "fs" "gs" "rs" "us"
        ))
    (save-excursion (let ((i -1))
    (insert "ASCII characters 0 thru 127.\n\n")
    (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
    (while (< i 31)
      (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                      (setq i (+ 1  i)) i (elt lower32 i)
                      (setq i (+ 32 i)) i (single-key-description i)
                      (setq i (+ 32 i)) i (single-key-description i)
                      (setq i (+ 32 i)) i (single-key-description i)))
      (setq i (- i 96))))))

(defun appleshan/show-current-buffer-major-mode ()
  (interactive)
  (describe-variable 'major-mode))

;; @see http://oremacs.com/2015/01/05/youtube-dl/
(defun appleshan/youtube-dl ()
  (interactive)
  (let* ((str (current-kill 0))
         (default-directory "~/download/media")
         (proc (get-buffer-process (ansi-term "/bin/bash"))))
    (term-send-string
     proc
     (concat "cd ~/download/media && youtube-dl " str "\n"))))

;; Configure network proxy
(defun show-proxy ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current proxy is \"%s\"" url-proxy-services)
    (message "No proxy")))

(defun set-proxy ()
  "Set http/https proxy."
  (interactive)
  (setq url-proxy-services `(("http" . "127.0.0.1:18080")
                             ("https" . "127.0.0.1:18080")))
  (show-proxy))

(defun unset-proxy ()
  "Unset http/https proxy."
  (interactive)
  (setq url-proxy-services nil)
  (show-proxy))

(defun toggle-proxy ()
  "Toggle http/https proxy."
  (interactive)
  (if url-proxy-services
      (unset-proxy)
    (set-proxy)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
