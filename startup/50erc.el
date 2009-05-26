;;; 50erc.el --- Set ERC options.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: irc local

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

(eval-after-load "erc"
  '(progn
     ;; Prevent these message types from triggering a display
     (setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))

     ;; Use faces for styling
     (setq erc-track-use-faces t)

     (erc-timestamp-mode t)
     (setq erc-timestamp-format "[%R-%m/%d]")

     (setq erc-user-full-name "Daniel Hackney")
     (setq erc-email-userid "dan@haxney.org")

     (setq erc-max-buffer-size 20000)

     (require 'erc-join)
     (erc-autojoin-mode 1)
     (setq erc-autojoin-channels-alist
           '(("freenode.net" "#drupal-vcs" "#drupal")))

     ;; Set the prompt to the channel name
     (setq erc-prompt
           (lambda ()
             (if (and (boundp 'erc-default-recipients) (erc-default-target))
                 (erc-propertize (concat (erc-default-target) ">")
                                 'read-only t 'rear-nonsticky t 'front-nonsticky t)
               (erc-propertize "ERC>"
                               'read-only t 'rear-nonsticky t 'front-nonsticky t))))

     (require 'notify)
     (defun dhackney-notify-erc (match-type nickuserhost message)
       "Notify when a message is received."
       (notify (format "%s in %s"
                       ;; Username of sender
                       (car (split-string nickuserhost "!"))
                       ;; Channel
                       (or (erc-default-target) "#unknown"))
               ;; Remove duplicate spaces
               (replace-regexp-in-string " +" " " message)
               :icon "emacs-snapshot"
               :timeout -1))
     (add-hook 'erc-text-matched-hook 'dhackney-notify-erc)

     ;; Respond once if mentioned while away
     (defvar erc-responded-once nil)
     (defvar erc-away-reason nil)
     (defun erc-respond-once-if-away (match-type nickuserhost msg)
       (if (erc-away-time)
           (if (eq match-type 'current-nick)
               (unless erc-responded-once
                 (erc-send-action (erc-default-target) (concat "is away: " erc-away-reason))
                 (setq erc-responded-once t)))))
     (add-hook 'erc-text-matched-hook 'erc-respond-once-if-away)

     (defadvice erc-process-away (after erc-away-reason-clear (proc away-p) activate)
       "Clear things"
       (unless away-p
         (setq erc-responded-once nil
               erc-away-reason nil)))

     (defadvice erc-cmd-AWAY (after erc-store-reason (line) activate)
       "store line"
       (when (string-match "^\\s-*\\(.*\\)$" line)
         (let ((reason (match-string 1 line)))
           (setq erc-away-reason reason))))

     ;; Load the passwords.
     (if (file-readable-p "~/.private/private.el")
         (load-file "~/.private/private.el"))))

(defun irc-maybe ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (erc :server "irc.freenode.net" :port 6667
         :nick "chrono325" :full-name "Daniel Hackney")))

;;; 50erc.el ends here
