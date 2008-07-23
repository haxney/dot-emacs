;;; 50count-words.el --- Provide `count-words-region' from the Emacs manual.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Provide `count-words-region' from the Emacs manual.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: convenience local

;; This file is NOT part of GNU Emacs.

;;; Code:
(defun count-words-region (beginning end)
  "Print number of words in the region."
  (interactive "r")
  (message "Counting words in region ... ")

  ;; 1. Set up appropriate conditions.
  (save-excursion
    (let ((count 0))
      (goto-char beginning)

      ;; 2. Run the while loop.
      (while (and (< (point) end)
                  (re-search-forward "\\w+\\W*" end t))
        (setq count (1+ count)))

      ;; 3. Send a message to the user.
      (cond ((zerop count)
             (message
              "The region does NOT have any words."))
            ((= 1 count)
             (message
              "The region has 1 word."))
            (t
             (message
              "The region has %d words." count))))))

(global-set-key "\C-c=" 'count-words-region)

;;; 50count-words.el ends here
