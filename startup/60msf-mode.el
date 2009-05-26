;;; 60msf-mode.el --- Set up `msf-abbrev'.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: msf-abbrev abbrev local

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

;;; Code:

(eval-after-load 'msf-abbrev
  '(progn

     ;; Need `org-mode-abbrev-table' in order to load abbrevs for it.
     (require 'org)

     ;; ensure abbrev mode is always on
     (setq-default abbrev-mode t)

     ;; do not bug me about saving my abbreviations
     (setq save-abbrevs nil)

     (setq msf-abbrev-verbose t)

     (setq msf-abbrev-root (concat
                            conf-home
                            (file-name-as-directory "mode-abbrevs")))
     (global-set-key (kbd "C-c m") 'msf-abbrev-define-new-abbrev-this-mode)
     (msf-abbrev-load)))

;;; 60msf-mode.el ends here
