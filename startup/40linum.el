;;; 40linum.el --- Code supporting `linum-mode'.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Code supporting `linum-mode'.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: linum lisp local

;; This file is NOT part of GNU Emacs.

;;; Code:

(defun linum-on ()
  "Ugly hack to turn off linum-mode for org-mode"
  (unless (or (minibufferp) (eq major-mode 'org-mode))
    (linum-mode 1)))

;;; 40linum.el ends here
