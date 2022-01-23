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


(defun ps-run (command)
  "Internal: run the powershell COMMAND.
If BUFFER not null the command output will be displayed into the BUFFER"
  (run-shc (format "powershell.exe %s" command)))

(defun ps-download-file (url filename)
  "Simple function for downloading a FILENAME fro URL."
  (ps-run (format "Invoke-WebRequest -Uri %s -OutFile %s" url filename)))

(defun ps-checksum (filename)
  "Simple function for verify the sha of FILENAME."
   (downcase (cdr (ps-run (format "(Get-FileHash %s -Algorithm SHA256)[0].Hash" filename)))))


(provide 'ps)
;;; ps.el ends here
