;;; 60flymake-tex.el --- Flymake settings for `tex-mode'

;; Copyright (C) 2008, Daniel Hackney

;; Description: Flymake settings for `tex-mode'.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: tex convenience local

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  Use chktex for latex flymake checking.

;;; Code:

(eval-after-load "flymake"
  '(progn
     ;; LaTeX
     (defun flymake-get-tex-args (file-name)
       (list "chktex" (list "-q" "-v0" file-name)))))

;;; 60flymake-tex.el ends here
