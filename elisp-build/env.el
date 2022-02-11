;;; env.el --- Environment for performing the build -*- lexical-binding: t; -*-
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

(defvar env-file-directory (file-name-directory (or load-file-name
                                                    (buffer-file-name)))
  "The directory of this file.")

(defvar env-eb-root (expand-file-name (concat env-file-directory "../"))
  "The root of this project.")

;;; TODO remove this
(defvar env-scripts (expand-file-name (concat env-eb-root "/scripts"))
  "The scripts folder.")

(defvar env-debug-buffer nil
  "Buffer to output the result of each shell-command.")

(defvar env-default-buffer "*Shell Command Output*"
  "Default buffer where the result of shell command are output.")

(defun env-add-to-load-path ()
  "Add this directory to the load path."
  (setq load-path (cons env-file-directory load-path)))

;;; Need this to make ~ working
(setenv "HOME" (getenv "USERPROFILE"))

;;; This is needed for pacman
(setenv "LANG" "C")

(env-add-to-load-path)

(provide 'env)
;;; env.el ends here