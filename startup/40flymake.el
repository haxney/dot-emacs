;;; 40flymake.el --- Set up common flymake options.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Set up common flymake options.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: convenience flymake

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  Load flymake if it is applicable to the current file.

;;; Code:

(add-hook 'find-file-hook 'flymake-find-file-hook)

;;; 40flymake.el ends here
