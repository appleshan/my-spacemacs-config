;;; config.el --- Chinese Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar *large-buffer-threshold* 300000
  "Buffer whose size beyond it will have a different behavior for the efficiency")

(defvar chinese-enable-avy-pinyin t
  "Enable ace-pinyin in avy-goto-char")

(defvar chinese-enable-fcitx nil
  "Enable fcitx to help writing Chinese in Evil mode.")
