;;; 50server.el --- Settings for Emacs server.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Settings for Emacs server.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: emacs-server local

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  Set up a special perspective for server files and switch to it when
;;  emacsclient opens. Also set up options for particular sites.

;;; Code:

;; Only start when the server is not already started.
(when (or (not (boundp 'server-process))
          (not (eq (process-status server-process)
                   'listen)))
  (server-start))

(defun server-perspective-switch ()
  "Open all server buffers in the \"server\" perspective.

This is intended to be a server hook."
  (let ((new-buf (switch-to-buffer (buffer-name)))
        (old-persp persp-curr-name))
    (persp-switch "server")
    (switch-to-buffer new-buf)
    (persp-switch old-persp)

    ;; Don't kill the buffer if we are already in the "server" perspective.
    (when (not (string-equal old-persp "server"))
      (persp-remove-buffer new-buf))
    (persp-switch "server")))

(defun server-edit-presets ()
  (cond
   ;; When editing mail, set the goal-column to 72.
   ((string-match "mail\\.google\\.com\\.[0-9a-z]+\\.txt" (buffer-name))
    (save-excursion
      (set-fill-column 72)
      (goto-char (point-min))
      ;; Replace strange space characters
      (while (search-forward " " nil t)
        (replace-match " "))))

   ;; Facebook textareas should not have hard newlines.
   ((string-match "www\\.\\(new\\.\\)?facebook\\.com\\.[0-9a-z]+\\.txt" (buffer-name))
    (longlines-mode))

   ;; Blogger wants longlines-mode as well.
   ((string-match "www\\.blogger\\.com\\.[0-9a-z]+\\.txt" (buffer-name))
    (longlines-mode))))

(add-hook 'server-switch-hook 'server-perspective-switch)

(add-hook 'server-visit-hook 'server-edit-presets)

;;; 50server.el ends here
