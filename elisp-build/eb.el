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
;; Package-Requires: ((emacs "27.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Emacs Build package for performing action
;;
;;; Code:

(defvar eb-file-directory (file-name-directory (or load-file-name
                                                (buffer-file-name)))
  "The directory of this file.")

(defun eb-add-to-load-path ()
  "Add this directory to the load path."
        (setq load-path (cons eb-file-directory load-path)))

;;; Need this to make ~ working
(setenv "HOME" (getenv "USERPROFILE"))

(eb-add-to-load-path)

(require 'cl-lib)
(require 'ebl)
(require 'ps)
(require 'msys2)

;; This doesn't belong here, this file should abstract the msys operations not the full installation of msys
(defun eb-perform-installation ()
  "Install msys2 install."
  ;;; Download the file only if we don't have it
  (if (not (file-exists-p (expand-file-name msys2-file)))
        (ps-download-file msys2-url msys2-file))
  (cond ((string-equal (ps-checksum msys2-file) msys2-checksum)
         (if (not (file-exists-p msys2-installation-dir))
             (msys2-install msys2-base-dir))
         (msys2--disable-pacman-disk-space msys2-installation-dir)
         (msys2-pacman-upgrade)
         (print "Killing the dll")
         (shell-command "taskkill /f /fi 'MODULES EQ msys-2.0.dll'")
         (print "Re-disable pacman disk space")
         (msys2--disable-pacman-disk-space msys2-installation-dir)
         (print "Re run the upgrade")
         (msys2-pacman-upgrade))
        (t (message (format "Checksum not matching please delete %s" (expand-file-name msys2-file))))))

;; (msys2--remove msys2-installation-dir)
;; (msys2-pacman-upgrade "**upgrade-buffer**")
;; (msys2-perform-installation)
;; (msys2-run "echo \"hi\"" "*Messages*")
(eb-perform-installation)

(provide 'eb)
;;; eb.el ends here
