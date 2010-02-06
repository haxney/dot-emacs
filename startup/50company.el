;;; 50company.el --- Settings for company-mode

;; Copyright (C) 2009 Daniel Hackney

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

;;; Commentary:
;;
;; Settings for company-mode

;;; Code:

(eval-after-load "company-semantic"
  '(add-to-list 'company-semantic-modes 'python-mode))

(eval-after-load "company"
  '(progn
;     (add-hook 'company-mode-hook 'predictive-mode)
;     (add-hook 'company-completion-finished-hook 'company-predictive-accept)
))

;; Notify predictive of an accepted completion
(defun company-predictive-accept (candidate)
  "Notify predictive that a completion has been accepted."
  (when (eq company-backend 'company-predictive)
    (run-hook-with-args 'predictive-accept-functions
                        company-prefix candidate current-prefix-arg)))

;;; 50company.el ends here
