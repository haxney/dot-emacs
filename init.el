;;; init.el --- Load configuration files.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Load configuration files.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: startup local

;; This file is NOT part of GNU Emacs.

;;; Code:

;; Load all of the files in `startup-dir'.
(debian-run-directories (concat (file-name-as-directory user-emacs-directory) "startup"))

(provide 'init-file)

;;; init.el ends here
