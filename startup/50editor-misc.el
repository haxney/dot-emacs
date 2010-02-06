;;; 50editor-misc.el --- Various editing settings which have no home.

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
;; Dumping ground for settings which are not substantial enough to warrant a
;; file to themselves.

;;; Code:

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'log-edit-hook 'flyspell-mode)

(add-hook 'mail-send-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(defun delete-weird-chars ()
  "Replace non-ASCII characters with their ASCII equivalents.

Replaces fancy characters in the current buffer. For example,
replaces “ with \"."
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

;; Load the private files.
(if (file-readable-p "~/.private/private.el")
    (load-file "~/.private/private.el"))

;; Live on the wild side.
(setq write-region-inhibit-fsync t)

(semantic-load-enable-gaudy-code-helpers)
(global-semantic-idle-completions-mode -1)

(defun message-startup-time ()
  "Display a message of how long Emacs took to start up, in milliseconds."
  (message "Emacs loaded in %dms"
           (/ (-
               (+
                (third after-init-time)
                (* 1000000
                   (second after-init-time)))
               (+
                (third before-init-time)
                (* 1000000
                   (second before-init-time))))
              1000)))

(global-undo-tree-mode)

(add-hook 'after-init-hook 'message-startup-time)

;;; 50editor-misc.el ends here
