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
       `(,(concat (expand-file-name "~") "/Projects/gsoc-2009/.*\\.\\(php[34s]?\\|module\\|install\\|inc\\|test\\)$")
         ,(concat (expand-file-name "~") "/Projects/gsoc-2009/TAGS"))
       `(,(concat (expand-file-name "~") "/\.emacs\.d/.*\.el")
         ,(concat (expand-file-name "~") "/\.emacs\.d/TAGS"))
       ))

(defun anything-imenu-or-etags ()
  "Run anything with imenu and etags.
Fill in the symbol at point by default."
  (interactive)
  (anything '(anything-c-source-imenu anything-c-source-etags-select)
            nil nil nil
            ;; select thing-at-point by default.
            (thing-at-point 'symbol)))

(global-set-key (kbd "M-.") 'anything-imenu-or-etags)

(global-set-key (kbd "M-?") 'anything-etags-select-from-here)

;;; 50etags.el.el ends here
