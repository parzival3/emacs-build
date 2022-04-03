;;; ps.el --- Library for handling powershell commands -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Enrico Tolotto
;;
;; Author: Enrico Tolotto <https://github.com/ento>
;; Maintainer: Enrico Tolotto <etolotto@gmail.com>
;; Created: januar 12, 2022
;; Modified: januar 12, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/ento/ps
;; Package-Requires: ((emacs "27.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Library for handling powershell commands
;;
;;; Code:

(require 'env)
(require 'run)

(defun ps-run (command &optional directory)
  "Run psowershell COMMAND in DIRECTORY."
  (let ((output (run-shc (format "powershell.exe %s" command) directory)))
    (if (not (equal (car output) 0))
        (error (format "Command failed with result %d: %s" (car output) (cdr output))))
    output))

(defun ps-run-res (command &optional directory)
  "Run COMMAND in DIRECTORY but only return the result."
  (car (ps-run command directory)))

(defun ps-run-output (command &optional directory)
  "Run COMMAND in DIRECTORY but only return the output buffer."
  (cdr (ps-run command directory)))

(defun ps-download-file (url filename)
  "Simple function for downloading a FILENAME fro URL."
  (ps-run (format "Invoke-WebRequest -Uri %s -OutFile %s" url filename)))

(defun ps-checksum (filename)
  "Simple function for verify the sha of FILENAME."
   (downcase (ps-run-output (prin1-to-string (format "(Get-FileHash %s -Algorithm SHA256)[0].Hash" filename)))))


(provide 'ps)
;;; ps.el ends here
