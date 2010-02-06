;;; 50nxml.el --- Nxml settings.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: xml convenience local

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;

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
