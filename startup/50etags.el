;;; 50etags.el --- Set up Etags.
;;
;; Author: Daniel Hackney
;; Copyright (C) 2009 Daniel Hackney

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

;;; Commentary:
;;
;; Use `etags-select' and `etags-table' to automatically find the correct tags
;; for the current file.

(require 'etags-select)
(require 'etags-table)

(setq etags-table-alist
      (list
       '("/home/dhackney/Projects/gsoc-2009/.*\\.\\(php[34s]?\\|module\\|install\\|inc\\)$"
         "/home/dhackney/Projects/gsoc-2009/TAGS")
       ))

(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

;;; 50etags.el.el ends here
