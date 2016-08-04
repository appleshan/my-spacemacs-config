;;; funcs.el --- appleshan-dired Layer functions File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun appleshan-dired/up-directory()
  "goto up directory and resue buffer"
  (interactive)
  (find-alternate-file ".."))

;; dired-x.el 允许忽略不感兴趣的文件，不让他们显示出来，并且可以使用 M-o 来方便地切换忽略与显示。
;; 有两个变量可以控制究竟忽略哪些文件：
;; 1. dired-omit-files 是一个正则表达式，凡是匹配这个正则表达式的文件就会被忽略掉，
;;    默认情况下会忽略自动保存的文件(以 # 开头)，当前目录和父目录(也就是 . 和 .. )。
;; 2. dired-omit-extensions 是一个字符串列表，凡是文件名结尾是这个列表中的某一个的时就会被忽略。
;;
;; 忽略掉以 . 开头的文件和文件夹，在 Linux 下面这通常表示隐藏文件和文件夹
(defun appleshan-dired/dired-hook ()
  ; omit all hidden file which starts with `.'
  (setq dired-omit-files "^#\\|^\\..*")
  ; initially omit unintrested files
  ; Ⓞ : http://graphemica.com/%E2%93%84
  (spacemacs|diminish dired-omit-mode " Ⓞ" " O"))

(with-eval-after-load 'dired-sort
  (defun appleshan-dired/dired-sort-hook ()
    (interactive)
    (make-local-variable  'dired-sort-map)
    (setq dired-sort-map (make-sparse-keymap))
    (define-key dired-mode-map "s" dired-sort-map)
    (define-key dired-sort-map "n" 'dired-sort-name)
    (define-key dired-sort-map "x" 'dired-sort-extension)
    (define-key dired-sort-map "s" 'dired-sort-size)
    (define-key dired-sort-map "t" 'dired-sort-time)))

(with-eval-after-load 'dired+
  (defun appleshan-dired/get-size ()
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-buffer
        (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
        (message
          "Size of all marked files: %s"
          (progn
            ; (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
            (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*总用量$")
            (match-string 1))))))

  ;; dired 文件管理器可以把目录优先排在前面。
  (defun appleshan-dired/list-dir-first ()
    "Dired sort hook to list directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
    (and (featurep 'xemacs)
         (fboundp 'dired-insert-set-properties)
         (dired-insert-set-properties (point-min) (point-max)))
    (set-buffer-modified-p nil))

  (defun appleshan-dired/dired-diff ()
    "Ediff marked files in dired or selected files in separate window"
    (interactive)
    (let* ((marked-files (dired-get-marked-files nil nil))
          (other-win (get-window-with-predicate
                      (lambda (window)
                        (with-current-buffer (window-buffer window)
                          (and (not (eq window (selected-window)))
                               (eq major-mode 'dired-mode))))))
          (other-marked-files (and other-win
                                   (with-current-buffer (window-buffer other-win)
                                     (dired-get-marked-files nil)))))
      (cond ((= (length marked-files) 2)
             (ediff-files (nth 0 marked-files)
                          (nth 1 marked-files)))
            ((= (length marked-files) 3)
             (ediff-files3 (nth 0 marked-files)
                           (nth 1 marked-files)
                           (nth 2 marked-files)
                           ))
            ((and (= (length marked-files) 1)
                  (= (length other-marked-files) 1))
             (ediff-files (nth 0 marked-files)
                          (nth 0 other-marked-files)))
            ((= (length marked-files) 1)
             (dired-diff))
            (t (error "mark exactly 2 files, at least 1 locally")))))

  (defun appleshan-dired/open-in-external-app ()
    "Open the current or  marked files in external app."
    (interactive)
    (let (doIt
          (myFileList
           (cond
            ((string-equal major-mode "dired-mode")
             (dired-get-marked-files))
            (t (list (buffer-file-name))) ) ) )
      (setq doIt (if (<= (length myFileList) 5)
                     t
                   (y-or-n-p "Open more than 5 files?") ) )
      (when doIt
        (mapc (lambda (fPath)
                (let ((process-connection-type nil))
                  (start-process "" nil "xdg-open" fPath)))
              myFileList))))

  (defun appleshan-dired/rsync (dest)
    "Using rsync in dired."
    (interactive
     (list
      (expand-file-name
       (read-file-name
        "Rsync to:"
        (dired-dwim-target-directory)))))
    ;; store all selected files into "files" list
    (let ((files (dired-get-marked-files
                  nil current-prefix-arg))
          ;; the rsync command
          (tmtxt/rsync-command
           "rsync -arvz --progress "))
      ;; add all selected file names as arguments
      ;; to the rsync command
      (dolist (file files)
        (setq tmtxt/rsync-command
              (concat tmtxt/rsync-command
                      (shell-quote-argument file)
                      " ")))
      ;; append the destination
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument dest)))
      ;; run the async shell command
      (async-shell-command tmtxt/rsync-command "*rsync*")
      ;; finally, switch to that window
      (other-window 1)))

)
