;;; 50org-mode.el --- Set up `org-mode'.

;; Copyright (C) 2009, 2012 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: org local

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

;; Set up clock in/out functionality, as well as convenience keybindings.

;;; Code:

(eval-when-compile (require 'cl))
(autoload 'org-read-date "org")

(defun org-open-day-page ()
  "Use `org-read-date' to prompt for a date, and open the day-page file matching that name."
  (interactive)
  (find-file (expand-file-name
              (concat (file-name-as-directory org-directory)
                      (replace-regexp-in-string "-" "." (org-read-date nil))
                      ".org"))))

(defun org-preprocess-radio-lists ()
  "Preprocess radio lists before exporting."
  (save-excursion
    (save-match-data
     (goto-char (point-min))
     (while (re-search-forward "^[ \t]*#\\+ORGLST:?" nil t)
       (forward-line 1)
       (when (org-at-item-p)
         (save-excursion
           (org-list-send-list t)))))))

(defun org-preprocess-radio-tables ()
  "Preprocess radio tables before exporting."
  (save-excursion
    (save-match-data
     (goto-char (point-min))
     (while (search-forward "#+ORGTBL:" nil t)
       (forward-line 1)
       (when (org-at-table-p)
         (orgtbl-send-table t))))))

(defun dhackney/org-link-to-project (link desc)
  "Prompt for a link between org files.

Makes linking between `org-mode' files easier."
  (interactive (list (concat
                      "file:"
                      (ido-completing-read
                       "Org File: "
                       (directory-files (file-name-directory (buffer-file-name))
                                        nil
                                        "^[^\.#].*\.org")))
                     (read-from-minibuffer "Desc: ")))
  (insert (org-make-link-string link desc)))

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "C-M-m") 'org-insert-heading-after-current)
     (define-key org-mode-map (kbd "C-c M-l") 'dhackney/org-link-to-project)

     ;; Custom agenda commands
     (setq org-agenda-custom-commands
           '(("p" tags "PROJECT-MAYBE-DONE" nil)
             ("m" tags "PROJECT&MAYBE" nil)))
     (org-add-link-type "tel" nil 'org-format-export-tel-link)

     (add-hook 'org-export-preprocess-after-macros-hook 'org-preprocess-radio-lists)
     (add-hook 'org-export-preprocess-after-macros-hook 'org-preprocess-radio-tables)))

(defun org-format-export-tel-link (path desc format)
  "Format a tel: link for export"
  (case format
    (html
     (format "<a href=\"%s\">%s</a>" path desc))
    (latex
     (format "\\href{tel:%s}{\\texttt{%s}}" path desc))))

;;; 50org-mode.el ends here
