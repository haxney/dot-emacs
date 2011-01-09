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
;; Provides some convenient functions for anything.el

;;; Code:

(remove-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)

;; Prevent flickering
(setq anything-save-configuration-functions
      '(set-window-configuration . current-window-configuration))

;; Bind different groups of `anything' sources to various keys of a map. To
;; reduce redundancy, items can be added to the let-binded list `bindings' and
;; then are substituted into the command when it is evaled. Yay macros!
(defvar anything-activate-map (make-sparse-keymap)
  "Keybindings for various anything commands.")

(defun anything-set-map (bindings)
  "Set `anything-activate-map' according to the list of BINDINGS passed in.
Must be in the form:

  '((\"o\" anything-c-source-occur))"
  (mapcar (lambda (item)
            (let* ((key-name (symbol-name (car item)))
                   (map-buffer (concat " *anything-map-" key-name "*")))
             (define-key anything-activate-map
               (read-kbd-macro key-name)
               `(lambda ()
                  (interactive)
                  (anything-other-buffer (list ,@(cdr item))
                                         ,(get-buffer-create map-buffer))))))
          bindings)
  (global-set-key (kbd "C-c j") anything-activate-map)
  (global-set-key (kbd "C-c C-j") anything-activate-map))

(defvar anything-activate-map-commands
  '((o anything-c-source-occur)
    (b anything-c-source-buffers+
       anything-c-source-buffer-not-found)
    (h anything-c-source-man-pages
       anything-c-source-info-pages
       anything-c-source-info-elisp)
    (f anything-c-source-ffap-line
       anything-c-source-ffap-guesser
       anything-c-source-recentf
       anything-c-source-buffers+
       anything-c-source-bookmarks
       anything-c-source-file-cache
       anything-c-source-files-in-current-dir+)
    (p anything-c-source-eproject-files
       anything-c-source-eproject-projects)
    (m anything-c-source-bm
       anything-c-source-bm-all
       anything-c-source-bm-add))
  "List of keys to map to anything sources.

Is a list of (KEY SOURCE...), where KEY is the key in the keymap
which activate the SOURCEs given. For example, the if the list
contained the element

    (b anything-c-source-buffers+
       anything-c-source-buffer-not-found)

pressing \"b\" from the keymap would run

    (anything '(anything-c-source-buffers+
                anything-c-source-buffer-not-found))

This allows for simple definition of a large number of sources
without being overly verbose.")

(eval-after-load "anything"
  '(progn
     (require 'anything-config)
     (anything-set-map anything-activate-map-commands)))

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

(defun anything-for-buffers+ ()
  "Preconfigured `anything' for buffer."
  (interactive)
  (anything-other-buffer
   '(anything-c-source-buffers+ anything-c-source-buffer-not-found)
   (get-buffer-create " *anything for buffers+*")))

(defun anything-semantic-or-imenu ()
  "Run anything with semantic or imenu.

If semantic is active in the current buffer, then use semantic
for generating tags, otherwise fall back to imenu. Fill in the
symbol at point by default."
  (interactive)
  (let ((tap (substring-no-properties (or (thing-at-point 'symbol) "")))
        (source (if (semantic-active-p)
                    'anything-c-source-semantic
                  'anything-c-source-imenu)))
    (push-mark)
    (anything source
              nil nil nil
              (unless (string= tap "") tap))))

(global-set-key (kbd "M-.") 'anything-semantic-or-imenu)
(define-key emacs-lisp-mode-map (kbd "M-.") 'anything-semantic-or-imenu)
(global-set-key (kbd "C-h a") 'anything-apropos)

(defun anything-get-visible-buffers (&optional minibuf all-frames)
  (let ((bufs (make-symbol "buffers")))
    (set bufs nil)
    (walk-windows '(lambda (wind)
                     (add-to-list bufs (window-buffer wind)))
                  minibuf
                  all-frames)
    (symbol-value bufs)))

(defun anything-c-buffer-list ()
  "Return the list of names of buffers with boring buffers filtered out.
Boring buffer names are specified by
`anything-c-boring-buffer-regexp'. The first buffer in the list
will be the last recently used buffer that is not visible in the
current frame."
  (save-window-excursion
    (anything-frame/window-configuration 'restore)
    (let* ((visible (anything-get-visible-buffers))
           (buffers (mapcar '(lambda (buf)
                               (if (memq buf visible)
                                   nil
                                 buf))
                            (buffer-list))))

      (setq buffers (delq nil buffers))
      (mapc '(lambda (buf) (setq buffers (delete buf buffers))) visible)
      (mapcar 'buffer-name buffers))))

(when (require 'descbinds-anything nil t)
  (global-set-key (kbd "C-x b") 'anything-for-buffers+)
  (descbinds-anything-install))

(setq anything-candidate-number-limit 500)

;;; 50anything.el ends here
