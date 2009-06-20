;;; 50elisp-update.el --- Automatically download and update elisp files.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: elisp files local

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
;; Maintain a list of elisp files to automatically update. Since emacswiki
;; enforces a download limit, only 10 files can be specified at a time.

;;; Code:

(require 'install-elisp)
(defvar install-elisp-update-files
  '((emacswiki . (
                  (1 . (
                        "anything-complete.el"
                        "anything.el"
                        "anything-etags.el"
                        "anything-match-plugin.el"
                        "anything-rcodetools.el"
                        "apache-mode.el"
                        "auto-install.el"
                        "descbinds-anything.el"
                        "etags-select.el"
                        "etags-table.el"
                        ))
                  (2 . (
                        "install-elisp.el"
                        "linkd.el"
                        "notify.el"
                        "php-completion.el"
                        "bm-ext.el"
                        ))))
    (other . (
              "http://stud4.tuwien.ac.at/~e0225855/linum/linum.el"
              "http://users.skynet.be/ppareit/projects/graphviz-dot-mode/graphviz-dot-mode.el"
              "http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el"
              )))
  "Elisp files to update.
Since emacswiki only allows downloading 10 files at a time, split
it into multiple lists.")


(defun auto-update-elisp (ewiki-index)
  (interactive '(1))
  "Automatically update elisp files.
EWIKI-INDEX specifies which set of 10 files to read from EmacsWiki."
  (mapcar (lambda (elem)
            (install-elisp-from-emacswiki elem))
          (cdr (assoc ewiki-index
                      (cdr (assoc 'emacswiki install-elisp-update-files)))))
  (mapcar (lambda (elem)
            (install-elisp elem))
          (cdr (assoc 'other install-elisp-update-files))))

;;; 50elisp-update.el ends here
