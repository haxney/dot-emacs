;;; 99timer-stop.el --- Stop the startup timer clock.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Stop the startup timer clock.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: lisp startup local

;; This file is NOT part of GNU Emacs.

;;; Code:

;; This gives us `third'
(require 'cl)

(let ((the-time (current-time)))
  (message "My .emacs loaded in %dms"
           (/ (-
               (+
                (third the-time)
                (* 1000000
                   (second the-time)))
               (+
                (third *emacs-load-start*)
                (* 1000000
                   (second *emacs-load-start*)))
               ) 1000)))

;;; 99timer-stop.el ends here
