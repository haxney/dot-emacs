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

;; Use anything for C-h b
(require 'descbinds-anything)
(descbinds-anything-install)

;; Prevent flickering
(setq anything-save-configuration-functions
    '(set-window-configuration . current-window-configuration))

;; Bind different groups of `anything' sources to various keys of a map. To
;; reduce redundancy, items can be added to the let-binded list `bindings' and
;; then are substituted into the command when it is evaled. Yay macros!
(defvar anything-activate-map
  (let ((bindings '(
                    ("o" anything-c-source-occur)
                    ))
        (map (make-sparse-keymap)))
    (mapcar (lambda (item)
              (define-key map (read-kbd-macro (car item))
                `(lambda ()
                   (interactive)
                   (anything (list ,@(cdr item))))))
            bindings)
    map)
  "Keybindings for various anything commands.")

;; The binding for starting all of my anything commands.
(global-set-key (kbd "C-c j") anything-activate-map)

;; Candidates
(defun anything-project-root-find-files (pattern)
  (when anything-project-root
    (start-process-shell-command "project-root-find"
                                 nil
                                 "find"
                                 anything-project-root
                                 (find-to-string
                                  `(and (prune (name "*.svn" "*.git"))
                                        (name ,(concat "*" pattern "*"))
                                        (type "f"))))))


(defvar anything-c-source-project-files
  '((name . "Project Files")
    (init . (lambda ()
              (unless project-details (project-root-fetch))
              (setq anything-project-root (cdr project-details))))
    (candidates . (lambda ()
                    (anything-project-root-find-files anything-pattern)))
    (type . file)
    (requires-pattern . 2)
    (volatile)
    (delayed)))


;;; 50anything.el ends here
