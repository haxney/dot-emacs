;;; 50pretty-lambda.el --- Use the lambda character in lisp modes.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Use the lambda character in lisp modes.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: lisp faces convenience local

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  Replace lambdas with special lambda character. Not yet ready to make it
;;  automatic.

;;; Code:

(defun pretty-lambdas ()
  (interactive)
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
		  (0 (progn (compose-region (match-beginning 1) (match-end 1)
									(make-char 'japanese-jisx0208 #x26 #x4B) )
					nil))))))

;;; 50pretty-lambda.el ends here
