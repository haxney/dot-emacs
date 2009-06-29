;;; 50nxhtml.el --- Load nXhtml.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: nxhtml local

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

;;; Code:

(load (concat conf-home
              (file-name-as-directory "elisp")
              (file-name-as-directory "nxhtml")
              "autostart.el"))

(c-add-style "drupal"
  '((c-basic-offset . 2)
    (c-offsets-alist . ((arglist-close . c-lineup-close-paren)
                        (case-label . +)
                        (arglist-intro . +)
                        (arglist-cont-nonempty . c-lineup-math)
                        (statement-cont . c-lineup-math)))))

(add-hook 'php-mode-hook '(lambda () (c-set-style "drupal")))
(add-hook 'php-mode-hook (lambda () (setq require-final-newline 'visit-save)))
(add-hook 'php-mode-hook '(lambda () (c-toggle-electric-state 1)))

;;; 50nxhtml.el ends here
