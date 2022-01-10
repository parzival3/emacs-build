;;; eb.el --- Emacs Build package for performing action -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Enrico Tolotto
;;
;; Author: Enrico Tolotto <https://github.com/ento>
;; Maintainer: Enrico Tolotto <etolotto@gmail.com>
;; Created: januar 10, 2022
;; Modified: januar 10, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/ento/eb
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Emacs Build package for performing action
;;
;;; Code:

(defun eb--buffer-content-string (buffer)
  "Internal function for getting the content of the BUFFER as a string"
   (with-current-buffer buffer
     (string-replace "\n" ""
                     (buffer-substring-no-properties (point-min) (point-max)))))

(defun eb--powershell (command &optional buffer)
  "Internal: run the powershell COMMAND.
If BUFFER not null the command output will be displayed into the BUFFER"
  (shell-command (format "powershell.exe %s" command) buffer))

(defun eb-download-file (url filename)
  "Simple function for downloading a FILENAME fro URL."
  (eb--powershell (format "Invoke-WebRequest -Uri %s -OutFile %s" url filename)))

(defun eb-checksum (filename)
  "Simple function for verify the sha of file."
  (let ((out-buffer "**sha-calc**"))
    (eb--powershell (format "(Get-FileHash %s -Algorithm SHA256)[0].Hash" filename) out-buffer)
    (downcase (eb--buffer-content-string out-buffer))))

(provide 'eb)
;;; eb.el ends here
