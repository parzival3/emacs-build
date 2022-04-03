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
(require 'env)
(require 'ps)
(require 'msys2)
(require 'pll)
(require 'sl)
(require 'fl)
(require 'pacman)

(defvar in-msys2-url "https://github.com/msys2/msys2-installer/releases/download/2021-11-30/msys2-base-x86_64-20211130.sfx.exe"
  "Url of the msys installation file.")

(defvar in-msys2-download-file "~/Downloads/msys.exe"
  "File location of the installation file.")

(defvar in-msys2-base-dir (file-name-as-directory (expand-file-name "C:/"))
  "Base directory where msys2 is installed.")

(defvar in-msys2-installation-dir (concat in-msys2-base-dir "msys64/")
  "Directory where msys2 is installed.")

(defvar in-msys2-dir ""
  "Directory of the installation configured by the environment variable
of msys2 installation.")

(defvar in-msys2-architecture ""
  "Installed msys2 system architecture.")

(defvar in-msys2-prefix ""
  "Msys2 prefix for packages to be installed in the system.")

(defvar in-msys2-build-type ""
  "Type of build performed by the current msys2 architecture.")

(defvar in-msys2-required-packages (list "zip" "unzip" "base-devel" "git" "autoconf")
  "List of msys2 packages for the build.")

(defvar in-emacs-repo "https://github.com/emacs-mirror/emacs.git"
  "The mirror for the Emacs repository.")

(defvar in-emacs-dir
  (file-name-as-directory (expand-file-name "~/emacs"))
  "Folder in which Emacs will be downloaded.")

(defvar in-emacs-branch "master"
  "Branch of Emacs to build.")

(defvar in-emacs-install-dir
  (file-name-as-directory (expand-file-name "~/emacs-install"))
  "Folder in which Emacs will be installed.")

(defvar in-emacs-cflags
  "-Ofast -fno-finite-math-only"
  "Default cflags for Emacs.")

(defvar in-emacs-platfrom-features "--disable-build-details --without-dbus"
  "Features that are only useful when building with mingw.")

(defvar in-build-threads 4
  "Default number of threads used for the compilation process.")

(defvar in-patch-directory (concat (file-name-directory
                                    (directory-file-name env-file-directory))
                                   "patches")
  "Directory containg the patches to be applied to Emacs before building it.")

(defvar in-emacs-pathes (directory-files in-patch-directory t ".patch")
  "List of patches to be applied to Emacs.")

(defvar in-emacs-features '((:xpm . :mingw-xpm-nox)
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

(defvar in-emacs-default-features '(:native-compilation :zlib :gnutls :xml2 :lcms2 :json :harfbuzz :rsvg :png :gif :jpeg :xpm)
  "List of default features of Emacs.")

(defvar in-emacs-default-removed-features '(:cairo :tiff :dbus :compress-install)
  "List of feature to exclude from Emacs.")

(defvar in-replacements-packages '((:mingw-libwinpthread-git . :mingw-libwinpthread)
                                  (:mingw-libtre-git . :mingw-libre))
  "Alist of packages that needs to be replace.")



;;; TODO: target, build and host should be the same as in-msys2 architecture.

(defun in-msys2-cmd-script (repository-root)
  "Script used to correctly setup the msys2 shell."
  (concat (file-name-as-directory repository-root) "/scripts/msys2.cmd"))

(defun in-install-msys2 (&rest config)
  "Install msys2 based on the CONFIG.
This step will extract the content of the file in-msys2-download-file
into the in-msys2-installation-dir."
  (let* ((directory (pll-dir config :msys2-installation-dir))
         (exe-file (pll-dir config :msys2-download-file))
         (command (format "%s -y" exe-file)))
    (ps-run command directory)))

(defun in-msys2-installed? (&rest config)
  "Check if the msys2 is already installed in the path specified in CONFIG."
  (let* ((directory (pll-dir config :msys2-installation-dir)))
    (file-exists-p directory)))

;;; TODO: fix this
(defun in-install-build-packages (prefix packages)
  "Install the toolchain for PREFIX plus the build PACKAGES."
  (msys2-pacman-install-pkgs (cons (concat prefix "-toolchain") packages)))

(defun in-msys2-downloaded? (&rest config)
  "check if the msys2 file described in CONFIG is already downlaoded."
  (let* ((msys2-file (pll-dir config :msys2-download-file)))
    (file-exists-p msys2-file)))

(defun in-downlaod-msys2 (&rest config)
  "Download the msys2 exe file using CONFIG."
  (let* ((msys2-url (pll-getr config :msys2-url))
         (msys2-file (pll-dir config :msys2-download-file))
         (msys2-checksum-url (concat msys2-url ".sha256"))
         (msys2-checksum-file (concat msys2-file ".sha256")))
    (ps-download-file msys2-url msys2-file)
    ;;; Check the integrity of the package
    (ps-download-file msys2-checksum-url msys2-checksum-file)
    (if (not (string-equal (fl-read-checksum msys2-checksum-file)
                           (ps-checksum msys2-file)))
        (error "The checksum for file %s downloaded from %s failed, please remove the file from the directory and retry" msys2-file msys2-url))))

(defun in-configure-pacman (&rest config)
  "Configure pacman before starting installing packages using CONFIG."
  (pacman-upgrade)
  (msys2-kill-dll)
  (pacman-disable-disk-space-checks (pll-dir config :msys2-installation-dir))
  (pacman-upgrade))

(defun in-configure-msys2 (&rest config)
  "Configure msys2 library in order to use a provided script using CONFIG."
  (msys2-init (pll-getr config :msys2-cmd-script)))

(defun in-symbol-to-package (symbol-package msys2-arch)
  "Convert a SYMBOL-PACKAGE to a string with the proper MSYS2-ARCH."
  (replace-regexp-in-string ":mingw" (concat "mingw-w64-" msys2-arch)
                            (symbol-name symbol-package)))

(defun in-requireq-packages-features (&rest config)
  "Use the list of features described in CONFIG for building Emacs."
  (cl-flet ((transformer (lambda (feature)
                           (in-symbol-to-package (cdr (assoc feature in-emacs-features))
                                                 (pll-getr config :msys2-arch)))))
    (mapcar #'transformer (pll-getr config :emacs-features))))



(defmacro in-perform-installation (&rest configuration)
  "Perform the full Emacs installtion following the CONFIGURATION."
  `(progn
     (when (not (in-msys2-downloaded? ,@configuration))
       (in-downlaod-msys2 ,@configuration))
     (when (not (in-msys2-installed? ,@configuration))
       (in-install-msys2  ,@configuration))
     ;; This is needed for setting up the msys2-run command
     (in-configure-msys2 ,@configuration)
     (in-configure-pacman ,@configuration)))


(in-perform-installation   :msys2-url "https://github.com/msys2/msys2-installer/releases/download/2021-11-30/msys2-base-x86_64-20211130.sfx.exe"
                           :msys2-arch "x86_64" ;; This can be either x86_64 or i686
                           :msys2-download-file "~/Downloads/msys.exe"
                           :msys2-installation-dir in-msys2-installation-dir
                           :msys2-cmd-script (concat env-eb-root "scripts/msys2.cmd")
                           :msys2-packages in-msys2-required-packages
                           :emacs-features in-emacs-default-features
                           :emacs-removed-features in-emacs-default-removed-features )



(provide 'in)
;;; in.el ends here
