;;; in.el --- Installer package responsible to handle the various steps -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Enrico Tolotto
;;
;; Author: Enrico Tolotto <https://github.com/ento>
;; Maintainer: Enrico Tolotto <etolotto@gmail.com>
;; Created: januar 23, 2022
;; Modified: januar 23, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/ento/in
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Installer. This packages is responsible to handle all the steps of the Emacs installation
;;
;;; Code:
(require 'ps)
(require 'msys2)
(require 'pll)
(require 'sl)

(defvar in-msys2-url "https://github.com/msys2/msys2-installer/releases/download/2021-11-30/msys2-base-x86_64-20211130.sfx.exe"
  "Url of the msys installation file.")

(defvar in-msys2-download-file "~/Downloads/msys.exe"
  "File location of the installation file.")

(defvar in-msys2-base-dir (file-name-as-directory (expand-file-name "C:/"))
  "Base directory where msys2 is installed.")

(defvar in-msys2-installation-dir (concat in-msys2-base-dir "msys64/")
  "Directory where msys2 is installed.")

(defvar in-msys2-dir ""
  "Directory of the msys2 installation configured by the environment variable of msys2 installation.")

(defvar in-msys2-architecture ""
  "Installed msys2 system architecture.")

(defvar in-msys2-prefix ""
  "Msys2 prefix for packages to be installed in the system.")

(defvar in-msys2-build-type ""
  "Type of build performed by the current msys2 architecture.")

(defvar in-msys2-required-packages (list "zip" "unzip" "base-devel" "git" "autoconf")
  "List of msys2 required packages for the build.")

;;; TODO: target, build and host should be the same as in-msys2 architecture.

(defun in-msys2-cmd-script (repository-root)
  "Script used to correctly setup the msys2 shell.
REPOSITORY-ROOT is the derictory of emacs-build."
  (concat (file-name-as-directory repository-root) "/scripts/msys2.cmd"))

(defun in-install-msys2 (&rest config)
  "Install msys2 based on the CONFIG.
This step will extract the content of the file in-msys2-download-file
into the in-msys2-installation-dir."
  (let* ((directory (pll-dir config :msys2-installation-dir))
         (exe-file (pll-dir config :msys2-download-file))
         (command (format "%s -o %s -y" exe-file directory)))
    (ps-run command)))

;;; TODO: fix this
(defun in-install-build-packages (prefix packages)
  "Install the toolchain for PREFIX plus the build PACKAGES."
  (msys2-pacman-install-pkgs (cons (concat prefix "-toolchain") packages)))

(defun in-download-msys2 (&rest config)
  "Download the msys2 exe file using CONFIG."
  (let* ((msys2-url (pll-getr config :msys2-url))
         (msys2-file (pll-dir config :msys2-download-file))
         (msys2-checksum-url (concat msys2-url ".sha256"))
         (msys2-checksum-file (concat msys2-file ".sha256")))
    (if (not (file-exists-p msys2-file))
        (ps-download-file msys2-url msys2-file))
  ;;; Check the integrity of the package
    (ps-download-file msys2-checksum-url msys2-checksum-file)
    (if (not (string-equal (car (sl-get-matches "^\\(.*?\\)[[:space:]]"
                                       (sl-file-content-string msys2-checksum-file) 1))
                  (ps-checksum msys2-file)))
        (error "The checksum for file %s downloaded from %s failed, please remove the file from the directory and retry" msys2-file msys2-url))))

;;; Tests
(in-download-msys2 :msys2-url "https://github.com/msys2/msys2-installer/releases/download/2021-11-30/msys2-base-x86_64-20211130.sfx.exe"
                  :msys2-download-file "~/Downloads/msys.exe"
                  :msys2-installation-dir in-msys2-installation-dir)

(in-install-msys2 :msys2-url "https://github.com/msys2/msys2-installer/releases/download/2021-11-30/msys2-base-x86_64-20211130.sfx.exe"
                  :msys2-download-file "~/Downloads/msys.exe"
                  :msys2-installation-dir in-msys2-installation-dir)

(provide 'in)
;;; in.el ends here
