;;; 40pgg.el --- Add sign-and-encrypt function to PGG.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Add sign-and-encrypt function to PGG.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: encrypt pgg local

;; This file is NOT part of GNU Emacs.

;;; Code:

(defun dhackney/pgg-encrypt-sign (rcpts &optional sign start end passphrase)
  "Sign and encrypt the buffer."
  (interactive (list (split-string (read-string "Recipients: ") "[ \t,]+") t))
  (let* ((start (or start (point-min)))
         (end (or end (point-max)))
         (status (pgg-encrypt-region start end rcpts sign passphrase)))
    (when (interactive-p)
      (pgg-display-output-buffer start end status))
    status))

(global-set-key (kbd "C-c / e") 'dhackney/pgg-encrypt-sign)

;;; 40pgg.el ends here
