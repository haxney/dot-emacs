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

;; Set up Smex
(require 'smex)
(eval-after-load "init.el" '(smex-initialize))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Fuzzy matching
(setq ido-enable-flex-matching t)

;;; 50ido.el ends here
