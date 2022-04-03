;;; pll.el --- Plist library -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Enrico Tolotto
;;
;; Author: Enrico Tolotto <https://github.com/ento>
;; Maintainer: Enrico Tolotto <etolotto@gmail.com>
;; Created: januar 23, 2022
;; Modified: januar 23, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/ento/pll
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Plist library
;;
;;; Code:

(defun pll-getr (plist prop)
  "Retrun the value of KEY stored in PLIST or throw an error."
  (if (not (plist-member plist prop))
      (error (format "plist %s doesn't contain value %s" (symbol-name plist) (symbol-name prop))))
  (plist-get plist prop))

(defun pll-dir (plist prop)
  "Retrun the value of KEY stored in PLIST and transform it into a file name."
  (expand-file-name (pll-getr plist prop)))

(provide 'pll)
;;; pll.el ends here
