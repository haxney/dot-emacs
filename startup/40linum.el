;;; 40linum.el --- Code supporting `linum-mode'.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Code supporting `linum-mode'.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: linum lisp local

;; This file is NOT part of GNU Emacs.

;;; Code:

(defadvice linum-on (around linum-not-for-org)
  "Do not turn on `linum-mode' if we are in `org-mode'."
  (unless (eq major-mode 'org-mode)
    ad-do-it))

(ad-activate 'linum-on)

;;; 40linum.el ends here
