;;; 60flymake-shell.el --- Flymake settings for `shell-mode'.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: shell flymake convenience settings

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

(eval-after-load "flymake"
  '(progn
     (defcustom flymake-shell-of-choice
       "/bin/bash"
       "Path of shell.")

     (defcustom flymake-shell-arguments
       (list "-n")
       "Shell arguments to invoke syntax checking.")

     (defconst flymake-allowed-shell-file-name-masks
       '(("\\.sh$" flymake-shell-init))
       "Filename extensions that switch on flymake-shell mode syntax checks.")

     (defcustom flymake-shell-err-line-pattern-re
       '(("^\\(.+\\): line \\([0-9]+\\): \\(.+\\)$" 1 2 nil 3))
       "Regexp matching JavaScript error messages.")

     (defun flymake-shell-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list flymake-shell-of-choice
               (append flymake-shell-arguments (list local-file)))))

     (defun flymake-shell-load ()
       (setq flymake-allowed-file-name-masks
             (append
              flymake-allowed-file-name-masks
              flymake-allowed-shell-file-name-masks))
       (setq flymake-err-line-patterns
             (append
              flymake-err-line-patterns
              flymake-shell-err-line-pattern-re))
       (flymake-mode t)
       (local-set-key (kbd "C-c d")
                      'flymake-display-err-menu-for-current-line))

     (add-hook 'sh-mode-hook 'flymake-shell-load)))

;;; 60flymake-shell.el ends here
