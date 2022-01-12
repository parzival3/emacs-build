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

(defun eb-shcc (command &optional buffer)
  "Emacs Build Shell Command Capture. Run a the COMMAND and output to BUFFER.
to the standard output if we are in batch mode."
  (cond (noninteractive
         (let ((result nil)
               (buffer (if (not buffer) "*eb-shell-command*" buffer)))
           (print (format "running: %s" command))
           (setq result (shell-command command buffer))
           (print (eb--buffer-content-string buffer))
           result))
        (t (shell-command command buffer))))

(defun eb-strip-new-line (string)
  "Strip the new line from the current STRING."
  (replace-regexp-in-string "\n" "" string))

(defun eb--buffer-content-string (buffer)
  "Internal function for getting the content of the BUFFER as a string."
   (with-current-buffer buffer
                     (buffer-substring-no-properties (point-min) (point-max))))

(defun eb--powershell (command &optional buffer)
  "Internal: run the powershell COMMAND.
If BUFFER not null the command output will be displayed into the BUFFER"
  (eb-shcc (format "powershell.exe %s" command) buffer))

(defun eb-download-file (url filename)
  "Simple function for downloading a FILENAME fro URL."
  (eb--powershell (format "Invoke-WebRequest -Uri %s -OutFile %s" url filename)))

(defun eb-checksum (filename)
  "Simple function for verify the sha of FILENAME."
  (let ((out-buffer "**sha-calc**"))
    (eb--powershell (format "(Get-FileHash %s -Algorithm SHA256)[0].Hash" filename) out-buffer)
    (downcase (eb-strip-new-line
               (eb--buffer-content-string out-buffer)))))
(provide 'eb)
;;; eb.el ends here
