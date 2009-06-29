;;; anything-eproject.el --- Anything integration for eproject.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: convenience project

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
;; Allows opening and closing eproject projects through anything, as well as
;; selection of files within a project.
;;
;; To activate, add anything-eproject.el to your load path and
;;
;;   (require 'anything-eproject)
;;
;; to your .emacs. You can then use the following sources
;;
;;  `anything-c-source-eproject-files'
;;    Search for files in the current eProject.
;; `anything-c-source-eproject-projects'
;;    Open or close eProject projects.
;;
;;
;; Eproject: http://github.com/jrockway/eproject
;; Anything: http://www.emacswiki.org/emacs/Anything

;;; Code:

(defvar anything-eproject-original-buffer nil)

(defvar anything-c-source-eproject-files
  '((name . "Files in eProject")
    (init . anything-c-source-eproject-files-init)
    (real-to-display . (lambda (real-name)
                         (replace-regexp-in-string (eproject-root anything-eproject-original-buffer)
                                                   ""
                                                   real-name)))
    (candidates-in-buffer)
    (type . file))
  "Search for files in the current eProject.")

(defun anything-c-source-eproject-files-init ()
  "Build `anything-candidate-buffer' of eproject files."
  (when eproject-mode
  (setq anything-eproject-original-buffer (current-buffer))
  (let ((project-files (eproject-list-project-files)))
  (with-current-buffer (anything-candidate-buffer 'local)
    (mapcar
     (lambda (item)
       (insert (expand-file-name item) "\n"))
     project-files)))))

(defvar anything-c-source-eproject-projects
  '((name . "Projects")
    (candidates . anything-c-source-eproject-projects-init)
    (action . (lambda (candidate)
                (find-file candidate))))
  "Open or close eProject projects.")

(defun anything-c-source-eproject-projects-init ()
  "Build `anything-candidate-buffer' of eproject projects."
  (mapcar (lambda (item)
            (car item)) eproject-attributes-alist))

(provide 'anything-eproject)

;;; anything-eproject.el ends here
