;;; 50pgg.el --- Add sign-and-encrypt function to PGG.

;; Copyright (C) 2008, Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: encrypt pgg local

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

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Pgg does not (or didn't) supply a combinged signing and ecryption function,
;; so add one.

;;; Code:

(defun esk/pgg-encrypt-sign (rcpts &optional sign start end passphrase)
  "Sign and encrypt the buffer or region (if the mark is active).

If the mark is active, sign and encrypt the region only,
otherwise sign and encrypt the whole buffer."
  (interactive
   (let ((string (split-string (read-string "Recipients: ") "[ \t,]+")))
     (list string
           t
           (if mark-active (region-beginning) (point-min))
           (if mark-active (region-end) (point-max)))))
  (let ((status (pgg-encrypt-region start end rcpts sign passphrase)))
    (when (interactive-p)
      (pgg-display-output-buffer start end status))
    status))

(global-set-key (kbd "C-c / e") 'esk/pgg-encrypt-sign)

;;; 50pgg.el ends here
