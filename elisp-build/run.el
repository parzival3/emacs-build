;;; run.el --- Library that handles all the shell commands -*- lexical-binding: t; -*-
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
(require 'sl)

;;; TODO: I don't really like the shape of this function
(defun run-shc (command &optional directory)
  "Run the shell COMMAND and return the result and the content of the command.
Optionally set the directory from where to run the command."
  (let* ((default-directory (if (not directory) default-directory directory))
         (output "")
         (result nil)
         (buffer (if (not env-debug-buffer) env-default-buffer env-debug-buffer)))
    (when noninteractive
      (print (format "running: %s" command)))
    (setq result (shell-command command buffer))
    (setq output (sl-buffer-content-string buffer))
    (when noninteractive
      ;; Print the output of the buffe
      (print output))
    (cons result output)))

(provide 'run)
;;; run.el ends here
