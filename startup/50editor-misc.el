;;; 50editor-misc.el --- Various editing settings which have no home.

;; Copyright (C) 2009 Daniel Hackney

;; Description: Various editing settings which have no home.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: convenience files local

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Dumping ground for settings which are not substantial enough to warrant a
;; file to themselves.

;;; Code:

(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(add-hook 'mail-send-hook 'delete-trailing-whitespace)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(add-to-list 'auto-mode-alist '("\\.module$" . nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . nxhtml-mumamo-mode))

(add-to-list 'auto-mode-alist '("\\.ss$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))

(add-to-list 'auto-mode-alist '("\\.gri$" . gri-mode))

(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;;; 50editor-misc.el ends here
