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
(require 'env)
(require 'cl-lib)
(require 'ebl)
(require 'ps)
(require 'msys2)
(require 'git)

(defvar eb-emacs-repo "https://github.com/emacs-mirror/emacs.git"
  "The mirror for the emacs repository")

(defvar eb-emacs-folder
  (expand-file-name "~/emacs"))

(defvar eb-emacs-branch "master"
  "Branch of Emacs to build.")

(defvar eb-patch-directory (concat (file-name-directory
				(directory-file-name env-file-directory))
			       "patches")
  "Directory containg the patches to be applied to Emacs before building it.")

(defvar eb-emacs-pathes (directory-files eb-patch-directory t ".patch")
  "List of patches to be applied to Emacs.")

(defvar eb-emacs-features '((:xpm . :mingw-xpm-nox)
			   (:jpeg . :mingw-libjpeg-turbo)
			   (:tiff . :mingw-libtiff)
			   (:gif . :mingw-giflib)
			   (:png . :mingw-libpng)
			   (:rsvg . :mingw-librsvg)
                           (:cairo . :mingw-cairo)
			   (:harfbuzz . :mingw-harfbuzz)
			   (:json . :mingw-jansson)
			   (:lcms2 . :mingw-lcms2)
			   (:xml2 . :mingw-libxml2)
			   (:gnutls . :mingw-gnutls)
			   (:zlib . :mingw-zlib)
			   (:native-compilation . :mingw-libgccjit))
  "List of features supported by Emacs.")

(defvar eb-emacs-default-features '(:native-compilation :zlib :gnutls :xml2 :lcms2 :json :harfbuzz :rsvg :png :gif :jpeg :xpm)
  "List of default features of Emacs.")

(defvar eb-emacs-default-removed-features '(:cairo :tiff :dbus :compress-install)
  "List of feature to exclude from Emacs.")

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

(defun eb-patch-emacs (list-of-patches &optional buffer)
  "Apply a LIST-OF-PATCHES to Emacs.
Optionally output the result to BUFFER."
  (while list-of-patches
    (git-apply-patch eb-emacs-folder (pop list-of-patches) buffer)))

(defun eb-prepare-source-dir ()
  "Run autogen inside the Emacs directory."
  (msys2-run-in-directory "./autogen.sh" eb-emacs-folder))

(defun eb-feature-to-packages (list-of-features)
  "Transform a LIST-OF-FEATURES into a list of packages."
  (cl-letf (transformer (lambda (feature)
			  (string-replace
			  (symbol-name
			   (assoc feature eb-emacs-features)


;;; Tests
;;; (msys2-pacman-install-pkg " mingw-w64-x86_64-libgccjit")
;;; (msys2-run-in-directory "./autogen.sh" eb-emacs-folder "autogen")
;;; (setq eb-emacs-patches (directory-files eb-patch-directory t ".patch"))
;;; (git-reset-repo eb-emacs-folder "reset")
;;; (git-apply-patch eb-emacs-folder (car eb-emacs-patches) "patch")
;;; (eb-patch-emacs eb-emacs-patches "**patch-buffer**")

(provide 'eb)
;;; eb.el ends here
