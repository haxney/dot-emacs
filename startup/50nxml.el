;;; 50nxml.el --- Nxml settings.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Nxml settings.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: xml convenience local

;; This file is NOT part of GNU Emacs.

;;; Code:

(eval-after-load "nxml-mode"
  '(progn
     ;; Spelling in nXML
     (require 'flyspell)
     (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)

     ;; Add new schemas to nXML
     (push (concat conf-home
                   (file-name-as-directory "schemas")
                   "schemas.xml")
           rng-schema-locating-files-default)))

;;; 50nxml.el ends here
