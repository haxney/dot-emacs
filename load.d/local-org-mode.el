;;; local-org-mode.el --- Set up `org-mode'.

;; Copyright (C) 2009, 2012, 2013 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: org local

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

;;; Code:

(defun org-open-day-page ()
  "Use `org-read-date' to prompt for a date, and open the day-page file matching that name."
  (interactive)
  (require 'org)
  (find-file (expand-file-name
              (concat (replace-regexp-in-string "-" "." (org-read-date nil))
                      ".org")
              org-directory)))

(defun org-format-export-tel-link (path desc format)
  "Format a tel: link for export"
  (case format
    (html
     (format "<a href=\"%s\">%s</a>" path desc))
    (latex
     (format "\\href{tel:%s}{\\texttt{%s}}" path desc))))

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "C-M-m") 'org-insert-heading-after-current)
     (org-add-link-type "tel" nil 'org-format-export-tel-link)))

;;; local-org-mode.el ends here
