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
    (b anything-c-source-buffers-list
       anything-c-source-buffer-not-found)
    (h anything-c-source-man-pages
       anything-c-source-info-pages
       anything-c-source-info-elisp)
    (f anything-c-source-ffap-line
       anything-c-source-ffap-guesser
       anything-c-source-recentf
       anything-c-source-buffers-list
       anything-c-source-bookmarks
       anything-c-source-file-cache
       anything-c-source-files-in-current-dir))
  "List of keys to map to anything sources.

Is a list of (KEY SOURCE...), where KEY is the key in the keymap
which activate the SOURCEs given. For example, the if the list
contained the element

    (b anything-c-source-buffers
       anything-c-source-buffer-not-found)

pressing \"b\" from the keymap would run

    (anything '(anything-c-source-buffers
                anything-c-source-buffer-not-found))

This allows for simple definition of a large number of sources
without being overly verbose.")

(eval-after-load "anything"
  '(progn
     (require 'anything-config)
     (anything-set-map anything-activate-map-commands)))

(defun anything-semantic-or-imenu ()
  "Run anything with semantic or imenu.

If semantic is active in the current buffer, then use semantic
for generating tags, otherwise fall back to imenu. Fill in the
symbol at point by default."
  (interactive)
  (let ((source (if (semantic-active-p)
                    'anything-c-source-semantic
                  'anything-c-source-imenu)))
    (push-mark)
    (anything :sources source
              :preselect (thing-at-point 'symbol))))

(defadvice anything-c-skip-current-buffer (before skip-visible-buffers
                                      (buffers)
                                      activate)
       "Skip all visible buffers, not just the current one."
       (save-window-excursion
         (anything-frame-or-window-configuration 'restore)
         (walk-windows '(lambda (wind)
                          (setq buffers (delete (buffer-name (window-buffer wind)) buffers))))))

(when (require 'descbinds-anything nil t)
  (descbinds-anything-install))

(setq anything-candidate-number-limit 50)

(defvar anything-frame nil)

(defun anything-initialize-frame ()
  (unless (and anything-frame (frame-live-p anything-frame))
    (setq anything-frame (make-frame '((name . "*Anything*")
                                       (width . 80)
                                       (height . 40)))))
  (select-frame anything-frame)

  (set-window-buffer (frame-selected-window anything-frame)
                     (get-buffer-create anything-buffer)))

(defun anything-hide-frame ()
  (when (and anything-frame (frame-live-p anything-frame))
    (make-frame-invisible anything-frame)))

;; Uncomment these to enable `anything' in a different frame.
;; (add-hook 'anything-after-initialize-hook 'anything-initialize-frame)
;; (add-hook 'anything-cleanup-hook 'anything-hide-frame)

(when (and (require 'anything nil t))
  (global-set-key (kbd "M-.") 'anything-semantic-or-imenu)
  (define-key emacs-lisp-mode-map (kbd "M-.") 'anything-semantic-or-imenu)
  (global-set-key (kbd "C-h a") 'anything-c-apropos)
  (global-set-key (kbd "C-x b") 'anything-buffers-list)
  (setq anything-idle-delay nil
        anything-input-idle-delay))

;;; 50anything.el ends here
