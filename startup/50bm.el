;;; 50bm.el --- Set up bm persistence.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: bookmarks convenience editing local

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
;; Make sure that bookmark information is loaded early and saved often.

;;; Code:

(setq bm-restore-repository-on-load t)

;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)

;; Saving bookmark data on killing a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save)

;; Saving the repository to file when on exit. kill-buffer-hook is not called
;; when emacs is killed, so we must save all bookmarks first.
(add-hook 'kill-emacs-hook 'bm-buffer-save-all)
(add-hook 'kill-emacs-hook 'bm-repository-save)

;; Update bookmark repository when saving the file.
(add-hook 'after-save-hook 'bm-buffer-save)

;; Restore bookmarks when buffer is reverted.
(add-hook 'after-revert-hook 'bm-buffer-restore)

;;; 50bm.el ends here
