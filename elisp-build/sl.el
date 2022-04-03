;;; sl.el --- String library -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Enrico Tolotto
;;
;; Author: Enrico Tolotto <https://github.com/ento>
;; Maintainer: Enrico Tolotto <etolotto@gmail.com>
;; Created: januar 10, 2022
;; Modified: januar 10, 2022
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/parzival3/eb
;; Package-Requires: ((emacs "27.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Emacs Build package for performing action
;;
;;; Code:
(require 'env)
(require 'subr-x)

(defun sl-buffer-content-string (buffer)
  "Function for getting the content of the BUFFER as a string."
  (string-trim
   (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max)))))

(defun sl-file-content-string (file)
  "Function for getting the content of the FILE as a string."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (sl-buffer-content-string (current-buffer))))

(defun sl-get-matches (regex input-string &optional n-match)
  "Apply the REGEX to INPUT-STRING and get the N-MATCH data."
  (save-match-data
    (let ((matches nil)
          (start 0))
      (while (string-match regex input-string start)
        (setq start (match-end 0))
        (setq matches (cons (match-string n-match input-string) matches)))
      (reverse matches))))

(provide 'sl)
;;; sl.el ends here
