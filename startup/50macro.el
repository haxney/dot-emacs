;;; 50macro.el --- Helper functions for emacs macros.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Helper functions for emacs macros.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: lisp

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  Macro creation with simple-macro-query is a little weird, since it both asks
;;  for the name of the prompt and again for the value to insert on that
;;  runthrough.

;;; Code:

;; From Emacs Wiki.
(defun simple-macro-query (arg)
  "Prompt for input using minibuffer during kbd macro execution.
    With prefix argument, allows you to select what prompt string
    to use. If the input is non-empty, it is inserted at point."
  (interactive "P")
  (let* ((prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
         (input (minibuffer-with-setup-hook (lambda () (kbd-macro-query t))
                  (read-from-minibuffer prompt))))
    (unless (string= "" input) (insert input))))

(global-set-key "\C-xQ" 'my-macro-query)

;;; 50macro.el ends here
