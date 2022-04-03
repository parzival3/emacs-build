;;; pacman.el --- Library for interacting with pacman package manager -*- lexical-binding: t; -*-
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

(defvar pacman-config-file "etc/pacman.conf"
  "Configuration file for pacman.")

(defvar pacman-install-args "-S --noconfirm --needed"
  "List of argument to use when installing a package.")

(defun pacman-run (command &optional directory)
  "Run msys2 COMMAND in DIRECTORY."
  (msys2-run command directory))

(defun pacman-run-res (command &optional directory)
  "Run COMMAND in DIRECTORY but only return the result."
  (car (pacman-run command directory)))

(defun pacman-run-output (command &optional directory)
  "Run COMMAND in DIRECTORY but only return the output buffer."
  (cdr (pacman-run command directory)))

(defun pacman-disable-disk-space-checks (installation-path)
  "Function for optimizing pacman config of msys2 installed in INSTALLATION-PATH."
  (let* ((pacman-conf (concat (expand-file-name installation-path) pacman-config-file))
         (backup-file (concat pacman-conf ".bk")))
    (save-match-data
      (with-temp-file pacman-conf
        (insert-file-contents-literally pacman-conf)
        ;; create backup file probably we don't need this in the final iteration
        (if (not (file-exists-p backup-file))
            (copy-file pacman-conf backup-file))
        (while  (search-forward "CheckSpace" nil t)
          (replace-match "#CheckSpace"))))))

(defun pacman-install-pkgs (packages)
  "Install a list of PACKAGES."
  (pacman-run (concat pacman-install-args " " (mapconcat #'identity packages " "))))

(defun pacman-upgrade (&optional buffer)
  "Pacman upgrade without confirmation &optional you can specify the output BUFFER."
  (pacman-run-res "--noprogressbar --noconfirm -Syuu" buffer))

(defun pacman-install-pkg (package)
  "Install a single PACKAGE."
  (if (consp package)
      (error "For a list of packages use msys-pacman-install-pkgs function instead")
    (pacman-install-pkgs (list package))))

(defun pacman-package-info (package)
  "Get the PACKAGE info.
PACKAGE can also be a list of packages."
  (cdr (pacman-run-output (format "-Qii %s" package))))

(defun pacman-get-deps (package)
  "Function for getting the dependencies of PACKAGE."
  (cl-flet ((remove-empty-and-suffix (lambda (package)
                                       (replace-regexp-in-string "[>=].*" ""
                                                                 (replace-regexp-in-string "None" "" package)))))
    (let ((package-description (pacman-package-info package)))
      (mapcar #'remove-empty-and-suffix
              (mapcan  #'split-string
                       (ebl-get-matches "Depends on[[:space:]]*: \\(.*\\)" package-description 1))))))

(defun pacman-get-deps-pkgs (packages)
  "Return the list of dependency for the list of PACKAGES."
  (let ((list-of-pkgs (mapconcat #'identity packages " ")))
    (pacman-get-deps list-of-pkgs)))

(provide 'pacman)
;;; pacman.el ends here
