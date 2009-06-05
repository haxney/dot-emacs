;;; 50smileys.el --- Set custom smileys.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: local

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
;; Emacs has trouble with transparent pngs so use xpm files instead.

;;; Code:

(defun smiley-wrap-regexp (smile-list)
  "Wrap the items of `smiley-regexp-alist' with \\( and \\)\\W.
This lets the specification be simpler and more readable."
  (mapcar (lambda (item)
            (append (list
                     (concat "\\("
                             (car item)
                             "\\)\\W"))
                    (cdr item)))
          smile-list))

;; Set the list. The strings are matched in order.
(let ((list-tmp
       '(("B-?[)D]" 1 "cool1")
         ("+/`\\\\" 1 "cowbell1")
         (":`(" 1 "cry1")
         ("\\}:-)" 1 "devil1")
         ("[:=]-?(" 1 "sad1")
         ("[:=]-?D" 1 "grin1")
         ("[:=]-?/" 1 "slant1")
         ("[:=]-?)" 1 "smile1")
         ("[:=]-?P" 1 "tongue1")
         (":(|)" 1 "monkey1")
         ("<3" 1 "heart1")
         (";[-^]?[)D]" 1 "wink1")
         ("\\\\m/" 1 "rockout1")
         (":-o" 1 "shocked1")
         ("[:=]-?|" 1 "straight_face1")
         (">\\.<" 1 "wince1")
         ("[xX]-?(" 1 "angry1"))))
  (setq smiley-regexp-alist
        (smiley-wrap-regexp list-tmp)))

;;; 50smileys.el ends here
