;;; ebl.el --- Emacs Build Library package for performing action -*- lexical-binding: t; -*-
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
(require 'subr-x)

(defun ebl-shcc (command &optional buffer)
  "Emacs Build Shell Command Capture. Run a the COMMAND and output to BUFFER.
to the standard output if we are in batch mode."
  (cond (noninteractive
         (let ((result nil)
               (buffer (if (not buffer) "*eb-shell-command*" buffer)))
           (print (format "running: %s" command))
           (setq result (shell-command command buffer))
           (print (ebl-buffer-content-string buffer))
           result))
        (t (shell-command command buffer))))

;;; TODO: finish this
(defun ebl-string-replace (from to string)
  "Find and replace the content of a string."
    (save-match-data
    (let ((start 0))
      (while (string-match from input-string start)
        (setq start (match-end 0))
        (setq matches (cons (replace-match to nil nil input-string)
      (reverse matches))))

      (while (search-forward "")))))

	
(defun ebl-buffer-content-string (buffer)
  "Function for getting the content of the BUFFER as a string."
   (with-current-buffer buffer
                     (buffer-substring-no-properties (point-min) (point-max))))

(defun ebl-shcc-output (command &optional buffer)
  "Similar to ebl-shcc but returns result of COMMAND and output in BUFFER."
  (let ((work-buffer (if (not buffer) (get-buffer-create "shcc-output")
		       (t buffer))))
    (with-current-buffer work-buffer
      (erase-buffer)
      (cons (ebl-shcc command work-buffer)
	    (string-trim
	     (ebl-buffer-content-string work-buffer))))))
		    

(provide 'ebl)
;;; ebl.el ends here
