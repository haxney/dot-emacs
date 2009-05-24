;;; 00load-path.el --- Set up load path for user libraries
;;
;; Filename: 00load-path.el
;; Description: Sets up load paths for user libraries.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Copyright (C) 2008, Daniel Hackney, all rights reserved.

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

;; Add local libraries to `load-path'.
(add-to-list 'load-path elisp-dir t)


;; Set up load paths for subdirectories of the 'elisp' directory.
(load-file (concat elisp-dir "elisp-paths.el"))

;; The storage location of temporary emacs files.
(setq conf-tmp (concat conf-home
                       (file-name-as-directory "tmp")))

;; Place semantic.cache files somewhere central
(setq semanticdb-default-save-directory
	  (concat conf-tmp "semantic-cache"))
