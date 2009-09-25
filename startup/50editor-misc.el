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

(add-to-list 'auto-mode-alist '("\\.module$" . nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.test$" . nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.install$" . nxhtml-mumamo-mode))

(add-to-list 'auto-mode-alist '("\\.ss$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))

(add-to-list 'auto-mode-alist '("\\.gri$" . gri-mode))

(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))

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

;; Load the private files.
(if (file-readable-p "~/.private/private.el")
    (load-file "~/.private/private.el"))

(semantic-load-enable-gaudy-code-helpers)

;;; 50editor-misc.el ends here
