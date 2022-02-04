;;; init.el --- Simple configuration
;;; Commentary:
;;; Simple Emacs configuration
;;; Code:

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;;; ------------------------------------------------------------------------------------------
(setq viper-mode 1)
(require 'viper)

;;; Look and feel
(load-theme 'modus-vivendi)
(show-paren-mode t)
(tool-bar-mode -1)
(column-number-mode)
;; Enables line numbers
(linum-mode 1)
(set-cursor-color "Green")
(scroll-bar-mode -1)
;; Add Hack as my default font
(defvar init-global-font "Hack-14"
  "Defines the global font to use.")

(add-to-list 'default-frame-alist '(font . init-global-font))
(set-face-attribute 'default t :font init-global-font)


;;; -------------------------------------------------------------------------------------------
(with-eval-after-load 'apropos
  (progn (require 'apropos)
	 (setq apropos-do-all t)))


(defvar init-backup-directory "C:\\Tmp\\"
  "Directory where to backup files.")

(cond ((eq system-type 'windows-nt)
       (setq init-backup-directory "C:\\Tmp\\")))

;; Save backup files inside default directory
(setq make-backup-file-name-function (lambda (file)
				       (make-directory init-backup-directory t)
				       (concat (file-name-as-directory init-backup-directory)
					       (file-name-nondirectory file) ".bk")))

(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (flymake-mode 1)))

(add-hook 'dired-mode-hook (lambda ()
			     (dired-hide-details-mode 1)))



;;; -------------------------------------------------------------------------------------------
;;; KEYBINDINGS
;; Macro workaround to make some commands work on the character
;; the cursor is on too
;; INIT-VIPER-ONE-CHAR-FORWARD
(defmacro init-viper-ocf (&rest body)
  "Wraps the BODY between `forward-char' and `backward-char'.
This is needed for viper to work properly"
  `(progn
     (forward-char)
     ,@body
     (backward-char)))

(defmacro init-viper-def-cmd (name args &rest body)
  "Define a wrapper for a command with ARGS named NAME and BODY.
This command will be execute it as if the cursor was one char forward
the current position.  Uses `do-one-char-forward'.
Use it like a defun without lambda-list."
  `(defun ,name (,@args)
     (interactive)
     (init-viper-ocf
      ,@body)))

(init-viper-def-cmd viper-imm-eval-last-sexp
  (&optional eval-last-sexp-arg-internal)
  (eval-last-sexp eval-last-sexp-arg-internal))

;; For now this should be good enough
(global-set-key (kbd "C-x C-e") #'viper-imm-eval-last-sexp)

;; Rebind C-M and homrow command for moving between s-exp
(global-set-key (kbd "C-M-j") #'forward-sexp)
(global-set-key (kbd "C-M-k") #'backward-sexp)
(global-set-key (kbd "C-M-h") #'backward-up-list)
(global-set-key (kbd "C-M-l") #'down-list)

;; Use UTF-8 config system
(prefer-coding-system 'utf-8-unix)

;; Stop using tabs
(setq-default indent-tabs-mode nil)

;; Remove trailing whitespaces after saving the file
(add-hook 'before-save-hook
          'delete-trailing-whitespace)


(defun capture-filename ()
  (let* ((root (project-root (project-current)))
         (file-name  (file-name-sans-extension buffer-file-name)))
         (concat (replace-regexp-in-string "/" ":" (file-relative-name file-name root)) ": ")))

(defun insert-preamble (preamble)
  (when (equal (buffer-name) "*vc-log*")
                   (insert preamble)))

(defun vc-log-advice (orig-fun &rest args)
  (let ((preamble (capture-filename)))
    (apply orig-fun args)
    (insert-preamble preamble)))

(advice-add 'vc-next-action :around #'vc-log-advice)


;; Set the indentation for cpp-mode
(setq c-basic-offset 4)
(c-add-style "microsoft"
             '("stroustrup"
               (c-offsets-alist
                (substatement-open . 0)
                (innamespace . +)
                (inline-open . 0)
                (inher-cont . c-lineup-multi-inher)
                (arglist-cont-nonempty . +)
                (template-args-cont . +))))

(setq c-default-style (cons '(c++-mode . "microsoft") c-default-style))

;; Useful functions for converting files
(defun dos2unix ()
  "Convert a DOS formatted text buffer to UNIX format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert a UNIX formatted text buffer to DOS format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; Reset the garbage collection threshlod
(setq gc-cons-threshold (* 2 1000 1000))

;; Load dci package
(load-file "C:\\Git\\dci-emacs\\dci.el")
(load-file "C:\\Git\\dci-emacs\\gaming.el")
(add-to-list 'load-path "c:/Git/groovy-emacs-modes/")
(add-to-list 'load-path "c:/Git/jenkins-mode/")
(add-to-list 'load-path "c:/Git/s.el/")
(add-to-list 'load-path "c:/Git/dash.el/")
;;; init.el ends here
