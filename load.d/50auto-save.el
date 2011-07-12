;;; 50auto-save.el --- Set up auto-save directory

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: lisp files convenience local

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
;;  Place autosave files in a global directory.

;;; Code:

(defvar autosave-dir
  (concat tmp-dir
          (file-name-as-directory "autosave"))
  "The directory in which to place auto-save (i.e. #foo#) files.")

(defun auto-save-file-name-p (filename)
  "Return non-nil if filename can be yielded by `make-auto-save-file-name'.
filename should lack slashes."
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))

;;; 50auto-save.el ends here
