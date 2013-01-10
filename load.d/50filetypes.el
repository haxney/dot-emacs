;;; 50filetypes.el --- Various editing settings which have no home.

;; Copyright (C) 2009 Daniel Hackney

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

(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

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
     (add-hook 'ruby-mode-hook 'ruby-electric-mode)))

(eval-after-load 'inf-ruby
  '(progn
     (setf (first inf-ruby-implementations) '("ruby" . "pry"))))

;; Make sure `haml-mode' has a higher priority than `nxhtml-mumamo-mode'.
(delete '("\\.haml$" . haml-mode) auto-mode-alist)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

(add-to-list 'auto-mode-alist '("\\.json\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(eval-after-load 'coffee-mode
  '(progn
     (setq-mode-local coffee-mode tab-width 2)))

(add-to-list 'auto-mode-alist '("\\.handlebars$" . mustache-mode))

(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(add-to-list 'auto-mode-alist '("\.cnf$" . conf-mode))

;; Live on the wild side.
(setq write-region-inhibit-fsync t)

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

(eval-after-load 'geiser-syntax
  '(progn
     (geiser-syntax--scheme-indent
      (define 3)
      (local 1)
      ;; Hacks for desugar. `type-case' won't cascade its indentation to
      ;; sub-forms
      (BracketLHS 1)
      (DotLHS 1)
      (IdLHS 1)
      (ObjectP 1)
      (DotP 1)
      (BracketP 1)
      (DotMethodP 1)
      (BrackMethodP 1)

      (FuncP 1)
      (AppP 1)
      (DefvarP 1)
      (DeffunP 1)
      (IdP 1)

      (WhileP 1)
      (ForP 1)

      (AssignP 1)

      (SeqP 1)
      (IfP 1)

      (NumP 1)
      (StrP 1)
      (TrueP 1)
      (FalseP 1)

      (PrimP 1)

      (PrimAssignP 1)

      (PreIncP 1)
      (PostIncP 1)
      (PreDecP 1)
      (PostDecP 1)

      (interp-result 1))))

(diminish 'abbrev-mode)

;;; 50filetyptes.el ends here
