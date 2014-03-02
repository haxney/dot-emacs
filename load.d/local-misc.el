;;; local-misc.el --- Miscellaneous functions

;; Copyright (C) 2013 Daniel Hackney

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

;;; Code:

(autoload 'global-company-mode "company" nil t)

(eval-after-load "company-semantic"
  '(progn
     '(add-to-list 'company-semantic-modes 'python-mode)
     '(add-to-list 'company-semantic-modes 'ruby-mode)))

(eval-after-load "company"
  '(progn
     (define-key company-active-map (kbd "RET") 'company-complete-selection)
     (define-key company-active-map (kbd "C-w") 'backward-kill-word)
     (defun company--good-prefix-p (prefix)
       (and (not (eq prefix 'stop))
            (or (company-explicit-action-p)
                (>= (or (cdr-safe prefix) (length prefix))
                    company-minimum-prefix-length))
            (stringp (or (car-safe prefix) prefix))))))

;;(ac-company-define-source ac-source-company-geiser geiser-company-backend)

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

(global-set-key (kbd "C-x Q") 'simple-macro-query)

(defun conditionally-enable-paredit-mode ()
  "Enable Paredit mode in the minibuffer during `eval-expression'"
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(defun pretty-print-xml (begin end)
  "Pretty format XML markup in region.

You need to have `nxml-mode'
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this. The function inserts linebreaks to separate tags that have
nothing but whitespace between them. It then indents the markup
by using nXML's indentation rules."
  (interactive (list (if mark-active (region-beginning) (point-min))
                     (if mark-active (region-end) (point-max))))
  (save-excursion
    (save-match-data
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))))

;; Re-enable narrow-to-region
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun really-activate-desktop ()
  "Activate `desktop-read' after init.
desktop.el insists on putting its `after-init-hook' lambda ahead
of my custom loading, so `desktop-save-mode' is not set when it
runs. By forcing the read to happen after loading the custom
file, we can make sure that `desktop-read' is actually called
when needed."
  (let ((key "--no-desktop"))
    (when (member key command-line-args)
      (setq command-line-args (delete key command-line-args))
      (setq desktop-save-mode nil)))
  (when desktop-save-mode
    (desktop-read)
    (setq inhibit-startup-screen t)))

(run-at-time t (* 60 10) 'desktop-save-in-desktop-dir)

;; Doing this seems to be important. Some stuff is not set up for customize to
;; act until after packages and such are loaded, but customize needs to set up
;; in order for those things to work. It's all very strange.
(add-hook 'after-init-hook 'really-activate-desktop 'append)

;; Elisp support in semantic
(require 'semantic/bovine/el nil t)

(defun server-edit-presets ()
  "Run some things when a server buffer is opened."
  (cond
   ;; When editing mail, set the goal-column to 72.
   ((string-match "mail\\.google\\.com\\.[0-9a-z]+\\.txt" (buffer-name))
    (org-mode)
    (auto-fill-mode)
    (setq fill-column 72)
    (save-excursion
      (goto-char (point-min))
      ;; Replace non-breaking strange space characters
      (while (search-forward (char-to-string 160) nil t)
        (replace-match " "))))
   ((string-match "reddit\\.com\\.[0-9a-z]+\\.txt" (buffer-name))
    (longlines-mode))
   ((string-match "/tmp/Rtmp.+" (buffer-file-name))
    (R-mode))
   ))

(c-add-style "drupal"
             '((c-basic-offset . 2)
               (c-offsets-alist . ((arglist-close . c-lineup-close-paren)
                                   (case-label . +)
                                   (arglist-intro . +)
                                   (arglist-cont-nonempty . c-lineup-arglist)))))

(defun c-style-drupal ()
  "Set the style to \"drupal\"."
  (interactive)
  (c-set-style "drupal"))

;;(add-to-list 'ac-sources 'ac-source-company-geiser)

(defun set-elisp-mode-name ()
  (setq mode-name "El"))

(add-hook 'doc-view-mode-hook 'less-minor-mode)

(remove-hook 'find-file-hooks 'vc-find-file-hook)

(provide 'local-misc)

;;; local-misc.el ends here
