;;; 50server.el --- Settings for Emacs server.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: emacs-server local

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
;;  Set up a special perspective for server files and switch to it when
;;  emacsclient opens. Also set up options for particular sites.

;;; Code:

;; Only start when the server is not already started.
(when (or (not (boundp 'server-process))
          (not (eq (process-status server-process)
                   'listen)))
  (server-start))

(defun server-edit-presets ()
  (cond
   ;; When editing mail, set the goal-column to 72.
   ((string-match "mail\\.google\\.com\\.[0-9a-z]+\\.txt" (buffer-name))
    (longlines-mode-off)
    (auto-fill-mode 1)
    (set-fill-column 72)
    (save-excursion
      (set-buffer (buffer-name))
      (goto-char (point-min))
      ;; Replace non-breaking strange space characters
      (while (search-forward (char-to-string 160) nil t)
        (replace-match " "))))))

(add-hook 'server-visit-hook 'server-edit-presets)
(add-hook 'server-visit-hook '(lambda () (longlines-mode 1)))

;;; 50server.el ends here
