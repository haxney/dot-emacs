;;; 50macro.el --- Helper functions for emacs macros.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: lisp

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
;;  Macro creation with simple-macro-query is a little weird, since it both asks
;;  for the name of the prompt and again for the value to insert on that
;;  runthrough.

;;; Code:

;; From Emacs Wiki.
(defun simple-macro-query (arg)
  "Prompt for input using minibuffer during kbd macro execution.
    With prefix argument, allows you to select what prompt string
    to use. If the input is non-empty, it is inserted at point."
  (interactive "P")
  (let* ((prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
         (input (minibuffer-with-setup-hook (lambda () (kbd-macro-query t))
                  (read-from-minibuffer prompt))))
    (unless (string= "" input) (insert input))))

(global-set-key "\C-xQ" 'simple-macro-query)

(defvar ballroom-levels '("Newcomer" "Bronze" "Silver" "Gold" "Open"))
(defvar ballroom-dances '("Cha Cha" "Rumba" "Swing" "Mambo" "Bolero"
                          "Waltz" "Tango" "Foxtrot" "Viennese Waltz" "Quckstep"
                          "Jive" "Samba" "Paso Doble"))

(defun ballroom-vid-title (comp year section &optional extension)
  "Generates a name for a ballroom video."
  (unless extension
    (setq extension ""))
  (let* ((level (ido-completing-read "Level: " ballroom-levels))
         (dance (ido-completing-read "Dance: " ballroom-dances))
         (round (ido-completing-read "Round: " '("1" "2" "3" "4" "Quarterfinals" "Semifinals" "Finals")))
         (heat (unless (member round '("Quarterfinals" "Semifinals" "Finals"))
                 (read-number "Heat: ")))
         (round-format (if heat
                           (format "Round %d, Heat %d" (string-to-int round) heat)
                         round)))
    (concat comp " " year " " level " " section " " dance " - " round-format extension)))

(defun ballroom-set-line (comp year section)
  "Insert a ballroom line into the current buffer."
  (dired-move-to-filename)
  (dired-do-shell-command "totem" nil (dired-get-marked-files t current-prefix-arg))
  (let ((extension (save-excursion
                     (re-search-forward "\\(\\..+\\)$" (point-at-eol))
                     (match-string-no-properties 1))))
    (kill-line)
    (insert (ballroom-vid-title comp year section extension)))
  (wdired-next-line 1))

(defun ballroom-rename-folder-interactive (comp year section)
  (interactive "sCompetition: \nnYear: \nsSection (American/International): ")
  (when (numberp year)
    (setq year (int-to-string year)))
  (while (not (eobp))
    (ballroom-set-line comp year section)))

;;; 50macro.el ends here
