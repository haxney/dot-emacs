;;; 50ido.el --- Set up `ido-mode'.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Set up `ido-mode'.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: ido convenience local

;; This file is NOT part of GNU Emacs.

;;; Code:

(when (string-match "^21\\." emacs-version)
  (require 'ido))

(ido-mode t)

;; Fuzzy matching
(setq ido-enable-flex-matching t)

;; Use Ido for M-x
(defun ido-execute ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (let (cmd-list)
       (mapatoms (lambda (S) (when (commandp S) (setq cmd-list (cons (format "%S" S) cmd-list)))))
       cmd-list)))))

(global-set-key "\C-x\C-m" 'ido-execute)

;;; 50ido.el ends here
