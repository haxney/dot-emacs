;;; 00load-path.el --- Set up load path for user libraries
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
;;  Sets up path variables needed for future steps.

;; The `~/.emacs.d" directory as a variable.
(setq conf-home (concat (file-name-as-directory (expand-file-name "~"))
						(file-name-as-directory ".emacs.d")))

;; The directory containing startup files
(setq startup-dir (concat conf-home
							  (file-name-as-directory "startup")))

;; The local elisp directory
(setq elisp-dir (concat conf-home
                                (file-name-as-directory "elisp")))

;; The package directory
(setq elpa-dir (concat conf-home
                          (file-name-as-directory "elpa")))

;; Add local libraries to `load-path'.
(add-to-list 'load-path elisp-dir t)
(add-to-list 'load-path elpa-dir t)

;; Set up load paths for subdirectories of the 'elisp' directory.
(load-file (concat elisp-dir "elisp-paths.el"))

;; The storage location of temporary emacs files.
(setq conf-tmp (concat conf-home
                       (file-name-as-directory "tmp")))

;; The directory to store all of the byte-compiled files.
(setq byte-comp-dir (concat conf-tmp
                            (file-name-as-directory "byte-cache")))

;;(add-to-list 'load-path byte-comp-dir)

(add-to-list 'load-path (concat elisp-dir
                                "auto-install"))

;; Place semantic.cache files somewhere central
(setq semanticdb-default-save-directory
	  (concat conf-tmp "semantic-cache"))

;;; 00load-path.el ends here
