;;; git.el --- package for git command -*- lexical-binding: t; -*-
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
(require 'env)
(require 'cl-lib)
(require 'ps)
(require 'ebl)
(require 'msys2)

(defun git-run (command &optional directory)
  "Run git COMMAND in DIRECTORY."
  (msys2-run (concat "git " command) directory))

(defun git-clone (repo &optional branch directory options buffer)
  "Clone the REPO in DIRECTORY using BRANCH.
Optionally set the additional OPTIONS and output BUFFER."
  (let* ((string-options (if (not options) "" options))
         (string-branch (if (not branch) "" (concat "-b " branch)))
         (string-dir (if (not directory) "" directory))
         (command (format "clone %s %s %s %s" string-options repo string-dir string-branch)))
    (git-run command buffer)))

(defun git-apply-patch (repo-directory patch &optional buffer)
  "Apply PATCH inside the REPO-DIRECTORY.
Optionally set the defualt BUFFER."
  (let* ((default-directory repo-directory)
         (default-args "--ignore-space-change --ignore-whitespace --inaccurate-eof")
         (command (format "apply %s %s" default-args patch)))
    (git-run command buffer)))

(defun git-reset-repo (repo-directory &optional buffer)
  "Reset hard the repo in REPO-DIRECTORY.
Optionally output the result in BUFFER."
  (let ((default-directory repo-directory))
    (git-run "reset --hard origin $HEAD" buffer)))

;;; Tests

;;; (git-clone "https://github.com/emacs-mirror/emacs.git" "master" "C:/emacs/" "--depth 1" "clone-emacs")


(provide 'git)
;;; git.el ends here
