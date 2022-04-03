;;; fl.el --- File utility library -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Enrico Tolotto
;;
;; Author: Enrico Tolotto <https://github.com/ento>
;; Maintainer: Enrico Tolotto <etolotto@gmail.com>
;; Created: januar 26, 2022
;; Modified: januar 26, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/ento/fl
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  File utility library
;;
;;; Code:

(require 'sl)

(defun fl-read-checksum (file)
  "Read the checksum contained in the FILE."
  (car (sl-get-matches "^\\(.*?\\)[[:space:]]"
                                       (sl-file-content-string file) 1)))

(provide 'fl)
;;; fl.el ends here
