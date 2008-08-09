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

     (unless (boundp 'rng-schema-locating-files-default)
       (setq rng-schema-locating-files-default '()))

     ;; Add new schemas to nXML
     (push (concat conf-home
                   (file-name-as-directory "schemas")
                   "schemas.xml")
           rng-schema-locating-files-default)
     (defun pretty-print-xml-region (begin end)
       "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
       (interactive "r")
       (save-excursion
         (nxml-mode)
         (goto-char begin)
         (while (search-forward-regexp "\>[ \\t]*\<" nil t)
           (backward-char) (insert "\n"))
         (indent-region begin end))
       (message "Ah, much better!"))
           ))

;;; 50nxml.el ends here
