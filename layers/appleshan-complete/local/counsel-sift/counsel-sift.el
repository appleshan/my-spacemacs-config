;;; counsel-sift.el --- Sift with ivy interface -*- lexical-binding: t -*-

;; Copyright (C) 2016 Apple Shan

;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;; Package-Version: 20160809.0001
;; Created: 2016-08-09
;; Keywords: search, sift
;; Version: 0.1
;; Package-Requires: ((counsel "0.8.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'counsel)

(defvar counsel-sift--search-commands
  '(("sift" . "sift --no-color --no-group -nr %s %S ."))
  "Alist of search commands and their corresponding commands
with options to run in the shell.")

(defvar counsel-sift--search-max-path-length 30
  "Truncate the current path in counsel search if it is longer
than this amount.")

;; search

(defvar counsel-sift--search-cmd)

(defun counsel-sift//make-counsel-search-function (tool)
  (lexical-let ((base-cmd
                 (cdr (assoc-string tool counsel-sift--search-commands))))
    (lambda (string &optional _pred &rest _unused)
      "Grep in the current directory for STRING."
      (if (< (length string) 3)
          (counsel-more-chars 3)
        (let* ((default-directory counsel--git-grep-dir)
               (args (if (string-match-p " -- " string)
                         (let ((split (split-string string " -- ")))
                           (prog1 (pop split)
                             (setq string (mapconcat #'identity split " -- "))))
                       ""))
               (regex (counsel-unquote-regex-parens
                       (setq ivy--old-re
                             (ivy--regex string)))))
          (setq counsel-sift--search-cmd (format base-cmd args regex))
          (spacemacs//counsel-async-command counsel-sift--search-cmd)
          nil)))))

(defun counsel-sift/counsel-search
    (&optional tools use-initial-input initial-directory)
  "Search using the first available tool in TOOLS. Default tool
to try is grep. If INPUT is non nil, use the region or the symbol
around point as the initial input. If DIR is non nil start in
that directory."
  (interactive)
  (require 'counsel)
  (letf* ((initial-input (if use-initial-input
                             (if (region-active-p)
                                 (buffer-substring-no-properties
                                  (region-beginning) (region-end))
                               (thing-at-point 'symbol t))
                           ""))
          (tool (catch 'tool
                  (dolist (tool tools)
                    (when (and (assoc-string tool counsel-sift--search-commands)
                               (executable-find tool))
                      (throw 'tool tool)))
                  (throw 'tool "grep"))))
    (if (not (executable-find "sift"))
        (message "Can't find %s command" (car (car counsel-sift--search-commands)))
      (setq counsel--git-grep-dir
            (or initial-directory
                (read-directory-name "Start from directory: ")))
      (ivy-read
       (concat ivy-count-format
               (format "%s from [%s]: "
                       tool
                       (if (< (length counsel--git-grep-dir)
                              counsel-sift--search-max-path-length)
                           counsel--git-grep-dir
                         (concat
                          "..." (substring counsel--git-grep-dir
                                           (- (length counsel--git-grep-dir)
                                              counsel-sift--search-max-path-length)
                                           (length counsel--git-grep-dir))))))
       (counsel-sift//make-counsel-search-function tool)
       :initial-input (rxt-quote-pcre initial-input)
       :dynamic-collection t
       :history 'counsel-git-grep-history
       :action #'counsel-git-grep-action
       :caller 'counsel-sift/counsel-search
       :keymap spacemacs--counsel-map
       :unwind (lambda ()
                 (counsel-delete-process)
                 (swiper--cleanup))))))

;; Define search functions for each tool
(cl-loop
   for (tools tool-name) in '(((list "sift") "sift"))
   do
   (eval
    `(progn
       (defun ,(intern (format "spacemacs/search-%s" tool-name)) ()
         ,(format
           "Use `counsel-sift/counsel-search' to search in the current
 directory with %s." (if (string= tool-name "auto")
                         "a tool selected from `dotspacemacs-search-tools'."
                       tool-name))
         (interactive)
         (counsel-sift/counsel-search ,tools))
       (defun ,(intern (format "spacemacs/search-%s-region-or-symbol"
                               tool-name)) ()
         ,(format
           "Use `counsel-sift/counsel-search' to search for
 the selected region or the symbol around point in the current
 directory with %s." (if (string= tool-name "auto")
                         "a tool selected from `dotspacemacs-search-tools'."
                       tool-name))
         (interactive)
         (counsel-sift/counsel-search ,tools t))
       (defun ,(intern (format "spacemacs/search-project-%s" tool-name)) ()
         ,(format
           "Use `counsel-sift/counsel-search' to search in the current
 project with %s." (if (string= tool-name "auto")
                       "a tool selected from `dotspacemacs-search-tools'."
                     tool-name))
         (interactive)
         (counsel-sift/counsel-search ,tools nil (projectile-project-root)))
       (defun ,(intern (format "spacemacs/search-project-%s-region-or-symbol"
                               tool-name)) ()
         ,(format
           "Use `counsel-sift/counsel-search' to search for
 the selected region or the symbol around point in the current
 project with %s." (if (string= tool-name "auto")
                       "a tool selected from `dotspacemacs-search-tools'."
                     tool-name))
         (interactive)
         (counsel-sift/counsel-search ,tools t (projectile-project-root))))))

(provide 'counsel-sift)

;;; counsel-sift.el ends here
