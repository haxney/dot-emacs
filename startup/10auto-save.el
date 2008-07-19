;;; 10auto-save.el --- Set up auto-save directory.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Set up auto-save directory.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: lisp files convenience local

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  Place autosave files in a global directory.

;;; Code:

(defvar autosave-dir
  (concat conf-tmp
          (file-name-as-directory "autosave"))
  "The directory in which to place auto-save (i.e. #foo#) files.")

(defun auto-save-file-name-p (filename)
  "Return non-nil if filename can be yielded by `make-auto-save-file-name'.
filename should lack slashes."
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))

;;; 10auto-save.el ends here
