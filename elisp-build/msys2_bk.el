;;; msys2.el --- Simple package for installing msys2 -*- lexical-binding: t; -*-
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
;; Package-Requires: ((emacs "27.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  simple package for installing msys2
;;
;;; Code:
(require 'cl-lib)
(require 'ps)

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

(defvar msys2-cmd
  (concat env-file-directory "../scripts/msys2.cmd"))

(defvar msys2-dir ""
  "Directory of the installation configured by the environment variable
of msys2 installation.")

(defvar msys2-architecture ""
  "Installed msys2 system architecture.")

(defvar msys2-prefix ""
  "Msys2 prefix for packages to be installed in the system.")

(defvar msys2-build-type ""
  "Type of build performed by the current msys2 architecture.")

;;; TODO: I should also use the toolchain as a requirement, but the toolchain needs to be
;;;       defined after the config-build function.
(defvar msys2-build-packages (list "zip" "unzip" "base-devel" "git" "autoconf")
  "List of software neccessary for each build.")

(defvar msys2-pacman-install-args "-S --noconfirm --needed"
  "List of argument to use when installing a package.")

(defun msys2--debug-shell ()
  "Function for debugging the msys2 shell."
  (interactive)
  (let ((explicit-shell-file-name (expand-file-name msys2-cmd)))
    (message (format "Running shell %s" explicit-shell-file-name))
    (shell)))

(defun msys2-run (command &optional buffer)
  "Run msys2 COMMAND and capture the output in the BUFFER."
  (msys2-run-in-directory command default-directory buffer))

(defun msys2-run-in-directory (command directory &optional buffer)
  "Run msys2 COMMAND in DIRECTORY.
Optionally output to BUFFER."
  (let* ((default-directory directory)
        (result (ebl-shcc (format "%s -c '%s'" (expand-file-name msys2-cmd) command) buffer)))
    (if (not (equal result 0))
        (let* ((out-buffer (if (not buffer) "*Shell Command Output*" buffer))
               (message (format "Command %s failed with %d.\n %s" command result (ebl-buffer-content-string buffer))))
          (print message)
          (error message)))))

(defun msys2-run-output (command &optional buffer)
  "Run msys2 COMMAND and capture the output of BUFFER."
  (ebl-shcc-output (format "%s -c '%s'" (expand-file-name msys2-cmd) command) buffer))

(defun msys2-pacman-upgrade (&optional buffer)
  "Pacman upgrade without confirmation &optional you can specify the output BUFFER."
  (msys2-pacman "--noprogressbar --noconfirm -Syuu" buffer))

(defun msys2-install (directory)
  "Install the msys2 command prompt in the DIRECTORY."
  (let ((default-directory directory))
    (ps-run (format "%s -y" msys2-file))))

(defun msys2--remove (directory)
  "Remove the msys2 installation, DIRECTORY should be the msys directory."
  (if (not (string-match "msys64" directory))
      (error "The path should contain the msys64 string")
    (delete-directory directory t)))

(defun msys2-pacman (command &optional buffer)
  "Run the pacman COMMAND in BUFFER."
  (msys2-run (concat "pacman " command) buffer))

(defun msys2-pacman-output (command &optional buffer)
  "Like msys2-pacman but capture the output of COMMAND in the BUFFER."
  (msys2-run-output (format "pacman %s" command) buffer))

(defun msys2-pacman-install-pkg (package)
  "Install a PACKAGE."
  (if (consp package)
      (error "For a list of packages use msys-pacman-install-pkgs function instead")
    (msys2-pacman-install-pkgs (list package))))

(defun msys2-pacman-install-pkgs (packages)
  "Install a list of PACKAGES."
  (msys2-pacman (concat msys2-pacman-install-args " "
                        (mapconcat #'identity packages " "))))

(defun msys2-pacman-package-info (package)
  "Get the PACKAGE info."
  (cdr (msys2-pacman-output (format "-Qii %s" package))))

(defun msys2-pacman-get-deps (package)
  "Function for getting the dependencies of PACKAGE."
  (cl-flet ((remove-empty-and-suffix (lambda (package)
                                       (replace-regexp-in-string "[>=].*" ""
                                        (replace-regexp-in-string "None" "" package)))))
    (let ((package-description (msys2-pacman-package-info package)))
      (mapcar #'remove-empty-and-suffix
              (mapcan  #'split-string
                       (ebl-get-matches "Depends on[[:space:]]*: \\(.*\\)" package-description 1))))))

(defun msys2-pacman-get-deps-pkgs (packages)
  "Return the list of dependency for the list of PACKAGES."
  (let ((list-of-pkgs (mapconcat #'identity packages " ")))
    (msys2-pacman-get-deps list-of-pkgs)))


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

(defun msys2-get-env-var (envvar)
  "Get the msys2 ENVVAR variable."
  (cdr (msys2-run-output (format "echo $%s" envvar))))

(defun msys2--get-architecture ()
  "Get the installed msys2 architecture."
  (msys2-get-env-var "MSYSTEM"))

(defun msys2-configure-build (architecture)
  "Configure current msys2 installation from the ARCHITECTURE type."
  (setq msys2-dir (file-name-as-directory
                   (msys2-get-env-var "MINGW_PREFIX/")))
  (cond ((string-equal architecture "MINGW32")
         (setq msys2-architecture "i686"
               msys2-prefix "mingw-w64-i686"
               msys2-build-type "i686-w64-mingw32"))
        ((string-equal architecture "MINGW64")
         (setq msys2-architecture "x86_64"
               msys2-prefix "mingw-w64-x86_64"
               msys2-build-type "x86_64-mingw32"))
        ((string-equal architecture "MSYSTEM")
         (error "This tool cannot be run from an MSYS shell.  Please use Mingw64 or Mingw32"))
        (t (error "Couldn't determine the MSYS architecture"))))

(defun msys2-install-build-packages (prefix packages)
  "Install the toolchain for PREFIX plus the build PACKAGES."
  (msys2-pacman-install-pkgs (cons (concat prefix "-toolchain") packages)))




;;; Tests ---------------------------------------------------------------------------------------------

(ert-deftest msys2-configure-build-tests ()
  (should (string-equal "x86_64-mingw32" (msys2-configure-build "MINGW64")))
  (should (string-equal "i686-w64-mingw32" (msys2-configure-build "MINGW32")))
  (should-error (msys2-configure-build ""))
  (should-error (msys2-configure-build "MSYSTEM")))

;;; (msys2-install-build-packages msys2-prefix (list "zip" "unzip" "base-devel" "git"))
;;; (msys2-pacman-install-pkg "zip")
;;(msys2-run-in-directory "ls -al" (expand-file-name "~/Desktop"))

(provide 'msys2)
;;; msys2.el ends here
