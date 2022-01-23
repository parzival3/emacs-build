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

(defun pacman-run 

(defun pacman-query-list (list-of-packages)
  "List the files owned by the LIST-OF-PACKAGES.
Since we are only interested on the list of files and not which packages they belong to
the output of the command will return a list of files."
  (msys2-run-output (
   
(provide 'pacman)
;;; pacman.el ends here
