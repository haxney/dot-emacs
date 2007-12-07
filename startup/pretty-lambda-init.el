;; Replace lambdas with special lambda character. Not yet ready to make it
;; automatic.
(defun pretty-lambdas ()
  (interactive)
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
		  (0 (progn (compose-region (match-beginning 1) (match-end 1)
									(make-char 'japanese-jisx0208 #x26 #x4B) )
					nil))))))
