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
  "The mirror for the Emacs repository.")

(defvar eb-emacs-dir
  (file-name-as-directory (expand-file-name "~/emacs"))
  "Folder in which Emacs will be downloaded.")

(defvar eb-emacs-branch "master"
  "Branch of Emacs to build.")

(defvar eb-emacs-install-dir
  (file-name-as-directory (expand-file-name "~/emacs-install"))
  "Folder in which Emacs will be installed.")

(defvar eb-emacs-cflags
  "-Ofast -fno-finite-math-only"
  "Default cflags for Emacs.")

(defvar eb-emacs-platfrom-features "--disable-build-details --without-dbus"
  "Features that are only useful when building with mingw.")

(defvar eb-build-threads 4
  "Default number of threads used for the compilation process.")

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

(defvar eb-replacements-packages '((:mingw-libwinpthread-git . :mingw-libwinpthread)
				  (:mingw-libtre-git . :mingw-libre))
  "Alist of packages that needs to be replace.")

(defun eb-patch-emacs (list-of-patches &optional buffer)
  "Apply a LIST-OF-PATCHES to Emacs.
Optionally output the result to BUFFER."
  (while list-of-patches
    (git-apply-patch eb-emacs-dir (pop list-of-patches) buffer)))

(defun eb-prepare-source-dir ()
  "Run autogen inside the Emacs directory."
  (msys2-run-in-directory "./autogen.sh" eb-emacs-dir))

(defun eb-feature-to-packages (list-of-features with/without)
  "Transform a LIST-OF-FEATURES into a list of feature WITH/WITHOUT Emacs."
  (let ((prefix (cond ((eq :with with/without) "--with-")
                      ((eq :without with/without) "--without-")
                      (t (error "Valid symbols are :with or :without")))))
    (cl-flet ((transformer (lambda (feature)
                             (replace-regexp-in-string ":" prefix
                                (symbol-name
                                (car (assoc feature eb-emacs-features)))))))
      (mapcar #'transformer list-of-features))))

(defun eb-symbol-to-package (symbol-package)
  "Convert a SYMBOL-PACKAGE to a string with the proper architecture."
  (replace-regexp-in-string ":mingw" msys2-prefix (symbol-name symbol-package)))

(defun eb-requireq-packages-features (list-of-features)
  "Transform a LIST-OF-FEATURES in required mingw packages."
  (cl-flet ((transformer (lambda (feature)
                           (eb-symbol-to-package (cdr (assoc feature eb-emacs-features))))))
    (mapcar #'transformer list-of-features)))

(defun eb-prepare-msys2 ()
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

(defun eb-configure-emacs (add-features remove-features inst-dir cflags &optional buffer)
  "Configure Emacs with ADD-FEATURES and REMOVE-FEATURES in INST-DIR with CFLAGS.
Optionally capture the result in BUFFER."
  (let* ((+features (mapconcat #'identity (eb-feature-to-packages add-features :with) " "))
         (-features (mapconcat #'identity (eb-feature-to-packages remove-features :without) " "))
         (features (format "%s %s %s" +features -features eb-emacs-platfrom-features))
         (command (format "./configure --prefix=%s %s CFLAGS=\"%s\" --target x86_64-w64-mingw32 --build x86_64-w64-mingw32 --host x86_64-w64-mingw32 " inst-dir features cflags)))
    (print command)
    (msys2-run-in-directory command eb-emacs-dir buffer)))

(defun eb-build-emacs ()
  "Build Emacs in the default build directory."
  (msys2-run-in-directory (format "make -j%s" eb-build-threads) eb-emacs-dir))

(defun eb-perform-installation ()
  (eb-prepare-msys2)
  ;;; TODO: fix this
  (msys2-configure-build (msys2--get-architecture))
  (msys2-install-build-packages msys2-prefix msys2-build-packages)
  (git-clone eb-emacs-repo eb-emacs-branch eb-emacs-dir)
  (eb-patch-emacs (cdr eb-emacs-pathes))
  (msys2-pacman-install-pkgs (eb-requireq-packages-features eb-emacs-default-features))
  (eb-prepare-source-dir)
  (eb-configure-emacs eb-emacs-default-features
                      eb-emacs-default-removed-features
                      eb-emacs-install-dir
                      eb-emacs-cflags)
  (eb-build-emacs)
  (eb-install-emacs)
  (eb-copy-missing-file-in-directory)
  (eb-strip-install))


(defun eb-install-emacs ()
  "Install Emacs in the default emacs-install-dir."
  (msys2-run-in-directory (format "make -j%s install" eb-build-threads) eb-emacs-dir))

(defun eb-copy-missing-file-in-directory ()
  "Finsih the Emacs installation by copying the missing files into the installation folder."
  (let ((libgmp-files (directory-files (concat msys2-installation-dir  msys2-dir "bin") t "libgmp")))
    ;;; Copy libgmp files
    (while libgmp-files
      (copy-file (pop libgmp-files) (concat eb-emacs-install-dir "bin/")))
    (make-directory (concat eb-emacs-install-dir "/usr/share/emacs/site-lisp/") t)
    (copy-file (concat eb-emacs-install-dir "/share/emacs/site-lisp/subdirs.el")
	       (concat eb-emacs-install-dir "/usr/share/emacs/site-lisp/subdirs.el"))
    (copy-file (concat env-scripts "/site-start.el")
	       (concat eb-emacs-install-dir "/share/emacs/site-lisp/"))))

(defun eb-strip-install ()
  "Strip Emacs binary to reduce the size of the installation."
  (let ((executables (directory-files (concat eb-emacs-install-dir "/bin") t ".exe")))
    (while executables
      (msys2-run (format "strip -g --strip-unneeded %s" (pop executables))))))

(defun eb-get-all-dependencies (packages)
  "Function that keep iterating until all the dependencies of PACKAGES are listed."
  (let ((new-packages packages)
	(old-packages ()))
    (while (cl-set-difference new-packages old-packages :test #'string-equal)
      (setq old-packages new-packages)
      (setq new-packages
	    (cl-remove-if-not (lambda (pkg) (and (not (string-equal pkg ""))(cl-search "mingw" pkg)))
			      (cl-remove-duplicates
			       (append new-packages
				       (msys2-pacman-get-deps-pkgs new-packages)) :test #'string-equal))))
    new-packages)) 



(defun eb-fix-dependencies (replacements packages)
  "Replace the packages in the REPLACEMENTS into the list of PACKAGES.
For example '((\"libwinpthread-git\" \"libwinpthread\"))."
  (let ((string-replacements (mapcar (lambda (pkg) (cons (eb-symbol-to-package (car pkg))
							 (eb-symbol-to-package (cdr pkg)))) replacements)))
    (cl-delete-duplicates
    (append (mapcar #'car string-replacements)
	    (cl-set-difference packages (mapcar #'cdr string-replacements) :test #'string-equal)) :test #'string-equal)))


(defun eb-package-dependencies (zip-file dependencies)
  "Packages all the required DEPENDENCIES in ZIP-FILE."
  
  (msys2-run (format "pacman -Ql %s" (mapconcat #'identity (eb-fix-dependencies eb-replacements-packages (eb-get-all-dependencies (eb-requireq-packages-features eb-emacs-default-features))) " "))) 
  )
;;; (MSYS2-pacman-get-deps-pkgs (eb-requireq-packages-features eb-emacs-default-features)) 
;;(eb-strip-install)

;;(eb-copy-missing-file-in-directory)
;;(eb-requireq-packages-features eb-emacs-default-features)
;;(eb-feature-to-packages eb-emacs-default-features :without)

;;; Tests
;;; (msys2-pacman-install-pkg " mingw-w64-x86_64-libgccjit")
;;; (msys2-run-in-directory "./autogen.sh" eb-emacs-dir "autogen")
;;; (setq eb-emacs-patches (directory-files eb-patch-directory t ".patch"))
;;; (git-reset-repo eb-emacs-dir "reset")
;;; (git-apply-patch eb-emacs-dir (car eb-emacs-patches) "patch")
;;; (eb-patch-emacs eb-emacs-patches "**patch-buffer**")
;; (msys2--remove msys2-installation-dir)
;; (msys2-pacman-upgrade "**upgrade-buffer**")
;; (msys2-perform-installation)
;; (msys2-run "echo \"hi\"" "*Messages*")
;; (eb-perform-installation)

(provide 'eb)
;;; eb.el ends here
