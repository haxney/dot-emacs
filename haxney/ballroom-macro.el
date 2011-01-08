;;; ballroom-macro.el --- Helper functions for emacs macros.

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
;; Eases naming of ballroom dance videos when using wdired mode.

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

;;; ballroom-macro.el ends here
