;;; 50persp.el --- Set up `perspective'.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: perspective local

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
;;  Restrict `switch-buffer' to other buffers within this perspective. Allow
;;  switching to buffers outside of this perspective by using
;;  `ido-show-all-buffers'.

;;; Code:

(require 'perspective)

;; Emacs doesn't have a `filter' function. That is whack.
(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun filter-persp-buffers ()
  (setq ido-temp-list
        (filter (lambda (x)
                  (member (get-buffer x) persp-curr-buffers))
                ido-temp-list)))

(add-hook 'ido-make-buffer-list-hook 'filter-persp-buffers)

(defun ido-show-all-buffers ()
  "Show all buffers, not just the ones in this perspective.

This is useful when trying to add existing buffers to a perspective."
  (interactive)
  (setq ido-cur-list (mapcar (lambda (x) (buffer-name x)) (buffer-list))
        ido-text-init ""
        ido-rescan t
        ido-exit 'keep)
  (exit-minibuffer))

(define-key ido-buffer-completion-map (kbd "C-r") 'ido-show-all-buffers)

;;; 50persp.el ends here
