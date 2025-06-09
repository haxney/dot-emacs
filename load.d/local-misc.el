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

;;; Commentary:
;;
;; Does more other stuff.

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

(remove-hook 'find-file-hooks 'vc-find-file-hook)

(when (file-exists-p "~/Private/private.el.gz.gpg")
  (load "~/Private/private.el.gz.gpg"))

(defvar my/org-display-inline-base64-buffer "*inline base64 display*"
  "Buffer containing the image info.")

(defun my/org-display-inline-base64 ()
  "Decode an example block as a base64 image."
  (interactive)
  (let* ((element (org-element-at-point))
         (type (org-element-type element))
         (encoded-text (org-element-property :value element)))
    (unless (eq type 'example-block)
      (user-error "Not in an example block"))

    (with-current-buffer (pop-to-buffer my/org-display-inline-base64-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (insert (base64-decode-string encoded-text))
      (image-mode))))


(bind-key "C-M-;" 'comment-dwim)

(eval-after-load 'comint
  '(defadvice comint-previous-input
     (around restore-comint-input-with-zero-prefix activate)
     "Make `comint-previous-input' restore the input with arg == 0"
     (if (and
          comint-input-ring-index
          comint-stored-incomplete-input
          (eq arg 0))
         (comint-restore-input)
       ad-do-it)))

(provide 'local-misc)

;;; local-misc.el ends here
