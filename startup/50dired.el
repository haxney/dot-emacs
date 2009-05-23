;;; 50dired.el --- Settings for Dired-mode.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Settings for Dired-mode.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: convenience keybindings local dired

;; This file is NOT part of GNU Emacs.

;;; Code:

;; use 'e' to edit filenames
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)))

;;; 50dired.el ends here
