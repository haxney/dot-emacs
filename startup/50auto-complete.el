;;; 50auto-complete.el --- Load auto-complete and set options.

;; Copyright (C) 2009 Daniel Hackney

;; Description: Load auto-complete and set options.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: convenience local

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;

;;; Code:

(require 'auto-complete)

(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;;; 50auto-complete.el ends here
