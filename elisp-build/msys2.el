;;; msys2.el --- simple package for installing msys2 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Enrico Tolotto
;;
;; Author: Enrico Tolotto <https://github.com/ento>
;; Maintainer: Enrico Tolotto <etolotto@gmail.com>
;; Created: januar 10, 2022
;; Modified: januar 10, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/ento/mys2-installation
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  simple package for installing msys2
;;
;;; Code:

(require 'eb)

(defvar msys2-url "https://github.com/msys2/msys2-installer/releases/download/2021-11-30/msys2-base-x86_64-20211130.sfx.exe"
  "Url of the msys installation file.")

(defvar msys2-file "~/Downloads/msys.exe"
  "File location of the installation file.")

(defvar msys2-checksum "971f247546d1c7f92e711650136004a4f54119ce3131fe558112e24d69d0d352"
  "Sha of msys installation file.")

(defvar msys2-base-dir "C:/"
  "Directory where msys2 is installed.")

(defvar msys2-installation-dir (concat msys2-base-dir "msys64/")
  "Directory where msys2 is installed.")

(defvar msys2-cmd "../scripts/msys2.cmd")

(defun msys2--debug-shell ()
  (interactive)
  (let ((explicit-shell-file-name (expand-file-name msys2-cmd)))
    (message (format "Running shell %s" explicit-shell-file-name))
    (shell)))

(defun msys2-run (command &optional buffer)
  "Run msys2 COMMAND and capture the output in the BUFFER."
  (let ((final-command (format "%s -c '%s'" (expand-file-name msys2-cmd) command)))
    ;; For debug  (message (format "This is the command %s" final-command))
    (shell-command final-command buffer)))

(defun msys2-pacman-upgrade (&optional buffer)
  "Run pacman upgrade without confirmation."
  (msys2-run "pacman --noprogressbar --noconfirm -Syuu" buffer))

(defun msys2-install (directory)
  "Install the msys2 command prompt in the DIRECTORY."
  (let ((default-directory directory))
    (eb--powershell (format "%s -y" msys2-file))))

(defun msys2--remove (directory)
  (delete-directory directory t))

(defun msys2--disable-pacman-disk-space (installation-path)
  "Function for optimizing pacman config of msys2 installed in INSTALLATION-PATH."
  (let* ((pacman-conf (concat (expand-file-name installation-path) "etc/pacman.conf"))
         (backup-file (concat pacman-conf ".bk")))
    (save-match-data
      (with-temp-file pacman-conf
            (insert-file-contents-literally pacman-conf)
            ;; create backup file probably we don't need this in the final iteration
            (if (not (file-exists-p backup-file))
                (copy-file pacman-conf backup-file))
            (while  (search-forward "CheckSpace" nil t)
              (replace-match "#CheckSpace"))))))

(defun msys2-perform-installation ()
  "Install msys2 install."
  ;;; Download the file only if we don't have it
  (if (not (file-exists-p msys2-file))
        (eb-download-file msys2-url msys2-file))
  (cond ((string-equal (eb-checksum msys2-file) msys2-checksum)
         (msys2-install msys2-base-dir)
         (msys2--disable-pacman-disk-space msys2-installation-dir)
         (msys2-pacman-upgrade "**upgrade-buffer**")
         (shell-command "taskkill /f /fi 'MODULES EQ msys-2.0.dll'")
         ;; after the upgrade we need to re-desable the packman disk space.
         (msys2--disable-pacman-disk-space msys2-installation-dir)
         (msys2-pacman-upgrade "**upgrade-buffer**"))
        (t (message (format "Checksum not matching please delete %s" (expand-file-name msys2-file))))))

;; (msys2--remove msys2-installation-dir)
;; (msys2-pacman-upgrade "**upgrade-buffer**")
;; (msys2-perform-installation)
(msys2-perform-installation)
;;; msys2.el ends here
