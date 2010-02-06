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

;;; Code:

;; Org mode keys
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(autoload 'org-read-date "org")

(require 'org-install)

(defun dhackney/org-open-day-page ()
  "Use `org-read-date' to prompt for a date, and open the
day-page file matching that name."
  (interactive)
  (find-file (expand-file-name
              (concat "~/org/"
                      (replace-regexp-in-string "-" "." (org-read-date nil))
                      ".org"))))

(add-to-list 'Info-directory-list (concat elisp-dir (file-name-as-directory "org-mode") "doc"))

(define-key global-map "\C-c\M-d" 'dhackney/org-open-day-page)

(eval-after-load 'org
  '(progn
     (require 'org-babel-init)
     (require 'org-babel-R)
     (require 'org-babel-ruby)
     (require 'org-babel-python)
     (org-babel-load-library-of-babel)
     ;; Allow indentation without having to go to the arrow keys
     (define-key org-mode-map (kbd "C-c C-x C-f") 'org-shiftmetaright)
     (define-key org-mode-map (kbd "C-c C-x C-b") 'org-shiftmetaleft)
     (define-key org-mode-map (kbd "C-M-m") 'org-insert-heading-after-current)
     (define-key org-mode-map (kbd "C-c C-x C-o") 'my/org-clock-out)
     (global-set-key (kbd "C-c C-x C-i") 'my/org-todo-starting)

     ;; Turn on Flyspell when loading org-mode
     (add-hook 'org-mode-hook 'turn-on-flyspell)

     ;; Custom agenda commands
     (setq org-agenda-custom-commands
           '(("p" tags "PROJECT-MAYBE-DONE" nil)
             ("m" tags "PROJECT&MAYBE" nil)))

     ;; Org-registry
     ;; Remember inbound links in org files.
     (require 'org-registry)
     (org-registry-initialize)
     (org-registry-insinuate)

     (defun my/org-clock-out ()
       (interactive)
       "Clock out of whatever task is currently STARTED."
       (save-excursion
         (set-buffer (marker-buffer org-clock-marker))
         (goto-char org-clock-marker)
         (org-todo "WAITING")))

     (defun my/org-todo-starting ()
       (interactive)
       "Mark the current task WAITING."
       (org-todo "STARTED"))

     ;; Add Sacha Chua's 'clock-in(out)-if-starting' functions
     (defun wicked/org-clock-in-if-starting ()
       "Clock in when the task is marked STARTED."
       (when (and (string= state "STARTED")
                  (not (string= last-state state)))
         (org-clock-in)))

     (add-hook 'org-after-todo-state-change-hook
               'wicked/org-clock-in-if-starting)

     (defun wicked/org-clock-out-if-waiting ()
       "Clock out when the task is marked WAITING."
       (when (and (string= state "WAITING")
                  (equal (marker-buffer org-clock-marker) (current-buffer))
                  (< (point) org-clock-marker)
                  (> (save-excursion (outline-next-heading) (point))
                     org-clock-marker)
                  (not (string= last-state state)))
         (org-clock-out)))

     (add-hook 'org-after-todo-state-change-hook
               'wicked/org-clock-out-if-waiting)

     ;; Make linking between org files easier
     (defun dhackney/org-link-to-project ()
       "Prompt for a link between org files."
       (interactive)
       (let ((link (concat
                    "file:"
                    (ido-completing-read
                     "Org File: "
                     (directory-files (file-name-directory (buffer-file-name))
                                      nil
                                      "^[^\.#].*\.org"))))
             (desc (read-from-minibuffer "Desc: ")))
         (insert (org-make-link-string link desc))))

     (define-key org-mode-map (kbd "C-c M-l") 'dhackney/org-link-to-project)

     (unless (boundp 'org-export-latex-classes)
       (setq org-export-latex-classes nil))
     (add-to-list 'org-export-latex-classes
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
                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
     (defun org-preprocess-radio-lists ()
       "Preprocess radio lists before exporting."
       (save-excursion
         (goto-char (point-min))
         (while (search-forward "#+ORGLST:" nil t)
           (forward-line 1)
           (org-list-send-list t))))

     (defun org-preprocess-radio-tables ()
       "Preprocess radio tables before exporting."
       (save-excursion
         (goto-char (point-min))
         (while (search-forward "#+ORGTBL:" nil t)
           (forward-line 1)
           (orgtbl-send-table t))))
     (add-hook 'org-export-preprocess-final-hook 'org-preprocess-radio-lists)
     (add-hook 'org-export-preprocess-final-hook 'org-preprocess-radio-tables)))

;;; 50org-mode.el ends here