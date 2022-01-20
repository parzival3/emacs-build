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

(require 'ebl)
(require 'subr-x)

(defun ps-run (command &optional buffer)
  "Internal: run the powershell COMMAND.
If BUFFER not null the command output will be displayed into the BUFFER"
  (ebl-shcc (format "powershell.exe %s" command) buffer))

(defun ps-download-file (url filename)
  "Simple function for downloading a FILENAME fro URL."
  (ps-run (format "Invoke-WebRequest -Uri %s -OutFile %s" url filename)))

(defun ps-checksum (filename)
  "Simple function for verify the sha of FILENAME."
  (let ((out-buffer "**sha-calc**"))
    (ps-run (format "(Get-FileHash %s -Algorithm SHA256)[0].Hash" filename) out-buffer)
    (downcase (string-trim (ebl-buffer-content-string out-buffer)))))

(provide 'ps)
;;; ps.el ends here
