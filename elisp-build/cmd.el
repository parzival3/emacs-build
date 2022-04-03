;;; cmd.el --- Cmd command library -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Enrico Tolotto
;;
;; Author: Enrico Tolotto <https://github.com/ento>
;; Maintainer: Enrico Tolotto <etolotto@gmail.com>
;; Created: januar 12, 2022
;; Modified: januar 12, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/ento/cmd
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Cmd command library
;;
;;; Code:
(require 'ebl)

(defun cmd-run (command &optional buffer)
  "Run the cmd COMMAND on BUFFER."
  (ebl-shcc command buffer))

(defun cmd-kill (program &rest arguments)
  "Cmd kill PROGRAM"
  (cmd-run (format "taskkill /f %s %s" arguments program)))

(defun cmd-function (&rest args)
  (defun cmd--create--command (prop)
    (let ((values (plist-get args prop)))
      (cond ((listp values) (mapconcat (lambda (x) (concat (symbol-name prop) " " x)) values))
            (t (concat (symbol-name prop) values)))))
  (let ((copy-args (copy-alist args)))
    (while copy-args)
        (message (pop copy-args))))

(provide 'cmd)
;;; cmd.el ends here
