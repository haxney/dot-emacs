;;; 50msf-mode.el --- Set up `msf-abbrev'.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Set up `msf-abbrev'.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: msf-abbrev abbrev local

;; This file is NOT part of GNU Emacs.

;;; Code:

;; ensure abbrev mode is always on
(setq-default abbrev-mode t)

;; do not bug me about saving my abbreviations
(setq save-abbrevs nil)

(setq msf-abbrev-verbose t)

(setq msf-abbrev-root (concat
                       conf-home
                       (file-name-as-directory "mode-abbrevs")))
(global-set-key (kbd "C-c m") 'msf-abbrev-define-new-abbrev-this-mode)
(msf-abbrev-load)

;;; 50msf-mode.el ends here
