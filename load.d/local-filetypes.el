;;; local-filetypes.el --- Various editing settings which have no home.

;; Copyright (C) 2009, 2013 Daniel Hackney

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
;; Dumping ground for settings which are not substantial enough to warrant a
;; file to themselves.

;;; Code:

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

(add-to-list 'auto-mode-alist '("\\.ss$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))
(add-hook 'scheme-mode-hook 'paredit-mode)
;;(add-to-list 'ac-sources 'ac-source-company-geiser)

(add-to-list 'auto-mode-alist '("\\.gri$" . gri-mode))
(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Buildfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("config.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.json_builder$" . ruby-mode))

(eval-after-load 'ruby
  '(progn
     (add-hook 'ruby-mode-hook 'flyspell-prog-mode)
     (add-hook 'ruby-mode-hook 'ruby-electric-mode)
     (add-hook 'ruby-mode-hook 'yard-mode)))

(eval-after-load 'inf-ruby
  '(progn
     (setf (car inf-ruby-implementations) '("ruby" . "pry"))))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars$" . mustache-mode))
(add-to-list 'auto-mode-alist '("\.cnf$" . conf-mode))

(defun set-elisp-mode-name ()
  (setq mode-name "El"))

;; Allow "/sudo:host:/etc/stuff" to sudo on a remote host
(eval-after-load 'tramp
  '(progn
     (add-to-list 'tramp-default-proxies-alist
                  '(nil "\\`root\\'" "/ssh:%h:"))
     (add-to-list 'tramp-default-proxies-alist
                  '((regexp-quote (system-name)) nil nil))))

(defun dired-launch-command ()
  "Open the file at point"
  (interactive)
  (org-open-file (dired-get-filename)))

(eval-after-load 'dired
  '(progn
     (require 'org) ;; for `org-open-file'
     (define-key dired-mode-map "r" 'dired-launch-command)))

(defun linum-off ()
  "Unconditionally turn `linum-mode' off"
  (linum-mode -1))

(add-hook 'woman-mode-hook 'less-minor-mode)
(add-hook 'woman-mode-hook 'scroll-lock-mode)
(add-hook 'woman-mode-hook 'linum-off)

(add-hook 'Man-mode-hook 'less-minor-mode)
(add-hook 'Man-mode-hook 'scroll-lock-mode)

(add-hook 'doc-view-mode-hook 'less-minor-mode)

(remove-hook 'find-file-hooks 'vc-find-file-hook)

(eval-after-load 'pcache
  '(setq pcache-directory "~/.emacs.d/tmp/pcache"))

(eval-after-load 'geben
  '(defadvice geben-dbgp-redirect-stream (around
                                          geben-output-inhibit-read-only
                                          activate)
     "Set `inhibit-read-only' during `geben-dbgp-redirect-stream'"
     (let ((inhibit-read-only t)
           (inhibit-modification-hooks t))
       ad-do-it)
     (set-buffer-modified-p nil)))

(autoload 'vbnet-mode "vbnet-mode" nil t)
(add-to-list 'auto-mode-alist '("\.bas$" . vbnet-mode))

(diminish 'abbrev-mode)

;;; local-filetyptes.el ends here
