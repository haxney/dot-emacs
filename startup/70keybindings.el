;;; 70keybindings.el --- Set up keybindings.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: convenience keybindings local

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
;; Commands which are either part of core Emacs or are too small to warrant
;; their own file for a single `global-set-key'.

;;; Code:

;; Set C-w to backward kill word and remap existing C-w to C-x C-k
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Use C-c k for kmacro keys
(global-set-key (kbd "C-c k") 'kmacro-keymap)

;; Globally set C-c C-v C-c to compile.
(global-set-key (kbd "C-c C-v C-c") 'compile)

;; Set F5 to replay last macro
(global-set-key [f5] 'call-last-kbd-macro)

;; Commenting
(global-set-key (kbd "C-M-;") 'comment-region)

;; Use meta with arrow keys for windmove.
(windmove-default-keybindings 'super)

(define-key global-map (kbd "C-c r") 'remember)

(define-key global-map (kbd "C-c i u") 'identica-update-status-interactive)
(define-key global-map (kbd "C-c i i") 'identica)
;;; 70keybindings.el ends here
