;;; 50edit-help.el --- Various helpers for general editing.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Various helpers for general editing.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: emacs local

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  This is a collection of random functions which do not really belong anywhere
;;  else. If there is somewhere better to put them, they should be moved to a
;;  different file.

;;; Code:

(defun delete-weird-chars ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Replace strange space characters
    (while (search-forward "“" nil t)
      (replace-match "\""))
    (goto-char (point-min))
    (while (search-forward "”" nil t)
      (replace-match "\""))
    (goto-char (point-min))
    (while (search-forward "’" nil t)
      (replace-match "'"))
    (goto-char (point-min))
    (while (search-forward "‘" nil t)
      (replace-match "'"))))

;; Re-enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;;; 50edit-help.el ends here
