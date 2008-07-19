;;; 50css-mode.el --- Set CSS-mode options.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Set CSS-mode options.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: css local

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  Set CSS style options.

;;; Code:

(eval-after-load "css-mode"
  '(progn
     (setq cssm-indent-function 'cssm-c-style-indenter)
     (setq cssm-indent-level 4)))

;;; 50css-mode.el ends here
