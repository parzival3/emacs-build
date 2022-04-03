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
(require 'pll)
(require 'run)

(defvar msys2-cmd ""
  "Command for running the msys2 shell.")

(defun msys2-init (msys2-script)
  "Function for configuring the msys2 package using MSYS2-SCRIPT."
  (setq msys2-cmd msys2-script))

(defun msys2--debug-shell ()
  "Function for debugging the msys2 shell."
  (interactive)
  (let ((explicit-shell-file-name (expand-file-name msys2-cmd)))
    (message (format "Running shell %s" explicit-shell-file-name))
    (shell)))

(defun msys2-run (command &optional directory)
  "Run msys2 COMMAND in DIRECTORY."
  (let ((output (run-shc (format "%s -c '%s'" (expand-file-name msys2-cmd) command) directory)))
    (if (not (equal (car output) 0))
        (error (format "Command failed with result %d: %s" (car output) (cdr output))))
    output))

(defun msys2-run-res (command &optional directory)
  "Run COMMAND in DIRECTORY but only return the result."
  (car (msys2-run command directory)))

(defun msys2-run-output (command &optional directory)
  "Run COMMAND in DIRECTORY but only return the output buffer."
  (cdr (msys2-run command directory)))

(defun msys2-get-env (envvar)
  "Get the msys2 ENVVAR variable."
  (msys2-run-output (format "echo $%s" envvar)))

(defun msys2--get-architecture ()
  "Get the installed msys2 architecture."
  (msys2-get-env "MSYSTEM"))

(defun msys2-kill-dll ()
  "Command to kill the msys2 dll before upgrading pacman."
  (car (run-shc "taskkill /f /fi 'MODULES EQ msys-2.0.dll'")))

;;; TODO: I don't like the shape of this function.
(defun msys2-configure-build (architecture)
  "Configure current msys2 installation from the ARCHITECTURE type."
  (let ((msys2-dir (file-name-as-directory
                   (msys2-get-env "MINGW_PREFIX/")))
        (msys2-arch "")
        (msys2-prefix "")
        (msys2-build-type ""))
    (cond ((string-equal architecture "MINGW32")
         (setq msys2-arch "i686"
               msys2-prefix "mingw-w64-i686"
               msys2-build-type "i686-w64-mingw32"))
        ((string-equal architecture "MINGW64")
         (setq msys2-arch "x86_64"
               msys2-prefix "mingw-w64-x86_64"
               msys2-build-type "x86_64-mingw32"))
        ((string-equal architecture "MSYSTEM")
         (error "This tool cannot be run from an MSYS shell.  Please use Mingw64 or Mingw32"))
        (t (error "Couldn't determine the MSYS architecture")))
    (list :msys2-dir msys2-dir :msys2-arch msys2-arch
          :msys2-prefix msys2-prefix :msys2-build-type msys2-build-type)))





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
