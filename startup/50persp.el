;;; 50persp.el --- Set up `perspective'.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Set up `perspective'.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: perspective local

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  Restrict `switch-buffer' to other buffers within this perspective. Allow
;;  switching to buffers outside of this perspective by using
;;  `ido-show-all-buffers'.

;;; Code:

(require 'perspective)

(defun ido-show-all-buffers ()
  "Show all buffers, not just the ones in this perspective.

This is useful when trying to add existing buffers to a perspective."
  (interactive)
  (setq ido-cur-list (mapcar (lambda (x) (buffer-name x)) (buffer-list))
        ido-text-init ""
        ido-rescan t
        ido-exit 'keep)
  (exit-minibuffer))

(define-key ido-buffer-completion-map "\C-r" 'ido-show-all-buffers)

;;; 50persp.el ends here
