;;; local-platform-compat.el --- Compatibility for wrong platforms

;; Copyright (C) 2018 Daniel Hackney

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
;; Workarounds for OS X being the bane of my existence.

;;; Code:


(when (eq system-type 'darwin)
  (setq insert-directory-program "gls")


  (add-to-list 'exec-path "/usr/local/bin"))

;;; local-platform-compat.el ends here
