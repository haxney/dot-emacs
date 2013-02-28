;;; local-misc.el --- Miscellaneous functions

;; Copyright (C) 2013 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: convenience files local

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

;;; Code:

(autoload 'global-company-mode "company" nil t)

(eval-after-load "company-semantic"
  '(progn
     '(add-to-list 'company-semantic-modes 'python-mode)
     '(add-to-list 'company-semantic-modes 'ruby-mode)))

(eval-after-load "company"
  '(progn
     (define-key company-active-map (kbd "RET") 'company-complete-selection)
     (define-key company-active-map (kbd "C-w") 'backward-kill-word)
     (defun company--good-prefix-p (prefix)
       (and (not (eq prefix 'stop))
            (or (company-explicit-action-p)
                (>= (or (cdr-safe prefix) (length prefix))
                    company-minimum-prefix-length))
            (stringp (or (car-safe prefix) prefix))))))

;;(ac-company-define-source ac-source-company-geiser geiser-company-backend)

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

(global-set-key (kbd "C-x Q") 'simple-macro-query)

;;; local-misc.el ends here
