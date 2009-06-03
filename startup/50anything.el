;;; 50anything.el --- Set up anything.el.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: convenience abbrev local matching

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

(require 'anything)
(require 'anything-config)
(require 'anything-etags)
(require 'anything-complete)
(require 'descbinds-anything)

;; For some reason this is not already created.
(defun anything-c-define-dummy-source (name func &rest other-attrib)
  `((name . ,name)
    (candidates "dummy")
    ,@other-attrib
    (filtered-candidate-transformer
     . (lambda (candidates source)
         (funcall ',func)))
    (requires-pattern . 1)
    (volatile)
    (category create)))

(defun anything-c-dummy-candidate ()
  ;; `source' is defined in filtered-candidate-transformer
  (list (cons (concat (assoc-default 'name source)
                      " '" anything-input "'")
              anything-input)))

(remove-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)

;; Prevent flickering
(setq anything-save-configuration-functions
      '(set-window-configuration . current-window-configuration))

(defvar anything-c-source-eproject-files
  '((name . "Files in eProject")
    (init . anything-c-source-eproject-files-init)
    (candidates-in-buffer)
    (type . file)
    (real-to-display . (lambda (real)
                         (if real
                             (cadr (split-string real
                                                 (concat
                                                  (expand-file-name (cadr prj-current)) "/")))))))
  "Search for files in the current eProject.")

(defun anything-c-source-eproject-files-init ()
  "Build `anything-candidate-buffer' of eproject files."
  (with-current-buffer (anything-candidate-buffer 'local)
    (mapcar
     (lambda (item)
       (insert (format "%s/%s\n" (cadr prj-current) (car item))))
     prj-files)))

(defvar anything-c-source-eproject-projects
  '((name . "Projects")
    (candidates . (lambda ()
                    (mapcar (lambda (item)
                              (car item))
                            prj-list)))
    (action ("Open Project" . (lambda (cand)
                                (eproject-open cand)))
            ("Close projcet" . (lambda (cand)
                                 (eproject-close)))))
  "Open or close eProject projects.")

;; Bind different groups of `anything' sources to various keys of a map. To
;; reduce redundancy, items can be added to the let-binded list `bindings' and
;; then are substituted into the command when it is evaled. Yay macros!
(defvar anything-activate-map (make-sparse-keymap) "Keybindings for various anything commands.")

(defun anything-set-map (bindings)
  "Set `anything-activate-map' according to the list of BINDINGS passed in.
Must be in the form:

  '((\"o\" anything-c-source-occur))"
  (mapcar (lambda (item)
            (define-key anything-activate-map
              (read-kbd-macro (symbol-name (car item)))
              `(lambda ()
                 (interactive)
                 (anything (list ,@(cdr item))))))
          bindings)
  (global-set-key (kbd "C-c j") anything-activate-map))

(anything-set-map
 '(
   (o anything-c-source-occur)
   (b anything-c-source-buffers
      anything-c-source-buffer-not-found
      anything-c-source-buffers+)
   (h ;anything-c-source-man-pages
    anything-c-source-info-pages
    anything-c-source-info-elisp
    anything-c-source-info-cl)
   (f anything-c-source-ffap-line
      anything-c-source-ffap-guesser
      anything-c-source-recentf
      anything-c-source-buffers+
      anything-c-source-bookmarks
      anything-c-source-file-cache
      anything-c-source-files-in-current-dir+)
   (p anything-c-source-eproject-files
      anything-c-source-eproject-projects)
   ))

;; More useful descbinds-anything
(setq descbinds-anything-actions
      '(("Execute" . descbinds-anything-action:execute)
        ("Find Function" . descbinds-anything-action:find-func)
        ("Describe Function" . descbinds-anything-action:describe)))

;; Redefined
(defun descbinds-anything-sources (buffer &optional prefix menus)
  (mapcar
   (lambda (section)
     (list
      (cons 'name (car section))
      (cons 'candidates (cdr section))
      (cons 'candidate-transformer
            (lambda (candidates)
              (mapcar
               (lambda (pair)
                 (cons (funcall descbinds-anything-candidate-formatter
                                (car pair) (cdr pair))
                       (cons (car pair)
                             (intern-soft (cdr pair)))))
               candidates)))
      (cons 'action descbinds-anything-actions)
      (cons 'action-transformer
            (lambda (actions candidate)
              (and (commandp (cdr candidate))
                   actions)))
      (cons 'persistent-action
            'descbinds-anything-action:describe)))
   (descbinds-anything-all-sections buffer prefix menus)))
(descbinds-anything-install)

;;; 50anything.el ends here
