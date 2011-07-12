;;; 50org-mode.el --- Set up `org-mode'.

;; Copyright (C) 2009 Daniel Hackney

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
;;
;;  Set up clock in/out functionality, as well as convenience keybindings.


;;; History:
;;

;;; Code:

(require 'org-install)

(defun org-open-day-page ()
  "Use `org-read-date' to prompt for a date, and open the day-page file matching that name."
  (interactive)
  (require 'org)
  (find-file (expand-file-name
              (concat (file-name-as-directory org-directory)
                      (replace-regexp-in-string "-" "." (org-read-date nil))
                      ".org"))))

(defun my/org-clock-out ()
  "Clock out of whatever task is currently STARTED."
  (interactive)
  (save-excursion
    (set-buffer (marker-buffer org-clock-marker))
    (goto-char org-clock-marker)
    (org-todo "WAITING")))

(defun my/org-todo-starting ()
  "Mark the current task WAITING."
  (interactive)
  (org-todo "STARTED"))

;; Add Sacha Chua's 'clock-in(out)-if-starting' functions
(defun wicked/org-clock-in-if-starting ()
  "Clock in when the task is marked STARTED."
  (when (and (string= state "STARTED")
             (not (string= last-state state)))
    (org-clock-in)))

(defun wicked/org-clock-out-if-waiting ()
  "Clock out when the task is marked WAITING."
  (when (and (string= state "WAITING")
             (equal (marker-buffer org-clock-marker) (current-buffer))
             (< (point) org-clock-marker)
             (> (save-excursion (outline-next-heading) (point))
                org-clock-marker)
             (not (string= last-state state)))
    (org-clock-out)))

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

(defvar org-export-latex-class-vita
  `("vita"
    ,(concat "\\documentclass[ComputerScience,10pt]{vita}\n"
             "\\usepackage{hyperref}\n"
             "\\usepackage[left=2cm,top=1cm,right=2cm]{geometry}\n"
             "\\usepackage{multicol}\n"
             "\\addtolength{\\columnsep}{-0.3in}\n"
             "\\addtolength{\\multicolsep}{-0.1in}\n"
             "\\usepackage{savetrees}\n"

             "\\usepackage[compact]{titlesec}\n"
             "\\titlespacing{\\section}{0pt}{*0}{*0}\n"
             "\\titlespacing{\\subsection}{0pt}{*0}{*0}\n"
             "\\titlespacing{\\subsubsection}{0pt}{*0}{*0}\n"

             "\\usepackage{comment}\n"
             "\\usepackage{setspace}\n"
             "\\singlespacing\n"

             "\\setlength{\\topsep}{-0.6in}\n"
             )
    ("\\section{%s \\hrulefill}" . "\\section*{%s \\hrulefill}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  "A resume class for exporting org-mode files as LaTeX.")

(eval-after-load 'org
  '(progn
     ;; Allow indentation without having to go to the arrow keys
     (define-key org-mode-map (kbd "C-c C-x C-f") 'org-shiftmetaright)
     (define-key org-mode-map (kbd "C-c C-x C-b") 'org-shiftmetaleft)
     (define-key org-mode-map (kbd "C-M-m") 'org-insert-heading-after-current)
     (define-key org-mode-map (kbd "C-c M-l") 'dhackney/org-link-to-project)
     (define-key org-mode-map (kbd "C-c C-x C-i") 'my/org-todo-starting)
     (define-key org-mode-map (kbd "C-c C-x C-o") 'my/org-clock-out)

     ;; Custom agenda commands
     (setq org-agenda-custom-commands
           '(("p" tags "PROJECT-MAYBE-DONE" nil)
             ("m" tags "PROJECT&MAYBE" nil)))

     ;; Org-registry
     ;; Remember inbound links in org files.
     ;; (require 'org-registry)
     ;; (org-registry-initialize)
     ;; (org-registry-insinuate)

     (unless (boundp 'org-export-latex-classes)
       (setq org-export-latex-classes nil))

     (eval-after-load 'org-export-latex
       '(progn
         (add-to-list 'org-export-latex-classes org-export-latex-class-vita)))
     (add-hook 'org-mode-hook 'turn-on-flyspell)
     (add-hook 'org-after-todo-state-change-hook 'wicked/org-clock-in-if-starting)
     (add-hook 'org-after-todo-state-change-hook 'wicked/org-clock-out-if-waiting)
     (add-hook 'org-export-preprocess-after-macros-hook 'org-preprocess-radio-lists)
     (add-hook 'org-export-preprocess-after-macros-hook 'org-preprocess-radio-tables)))

;;; 50org-mode.el ends here
