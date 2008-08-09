;;; 50org-mode.el --- Set up `org-mode'.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Set up `org-mode'.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: org local

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  Set up clock in/out functionality, as well as convenience keybindings.

;;; Code:

;; Org mode keys
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(eval-after-load 'org
  '(progn
     ;; Allow indentation without having to go to the arrow keys
     (define-key org-mode-map "\C-c\C-x\C-f" 'org-shiftmetaright)
     (define-key org-mode-map "\C-c\C-x\C-b" 'org-shiftmetaleft)
     (define-key org-mode-map "\C-\M-m" 'org-insert-heading-after-current)
     (define-key org-mode-map "\C-c\C-x\C-o" 'my/org-clock-out)
     (define-key global-map "\C-c\C-x\C-i" 'my/org-todo-starting)

     ;; Turn on Flyspell when loading org-mode
     (add-hook 'org-mode-hook 'flyspell-mode)

     ;; Custom agenda commands
     (setq org-agenda-custom-commands
           '(("p" tags "PROJECT-MAYBE-DONE" nil)
             ("m" tags "PROJECT&MAYBE" nil)
             ))

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

     (define-key org-mode-map "\C-c\M-l" 'dhackney/org-link-to-project)
     ))

;;; 50org-mode.el ends here
