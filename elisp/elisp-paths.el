;;; elisp-paths.el --- Add subdirectories to `load-path'.

;; Copyright (C) 2009, Daniel Hackney

;; Description: Add subdirectories to `load-path'.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: local elisp

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  Modify `elisp-paths' as new libraries are added to the 'elisp' folder.

;;; Code:

(defvar elisp-paths '("org-mode/lisp"
                      "org-mode/contrib/lisp"
                      "smex"
                      "smart-tab")
  "A list of additional subdirectories in 'elisp' to add to the load path.")

;; Add `elisp-paths' to `load-path'
(setq load-path
      (append load-path
              (mapcar (lambda (item)
                        (concat elisp-dir
                                (file-name-as-directory item)))
                      elisp-paths)))

;;; elisp-paths.el ends here
