;;; 50erc.el --- Set ERC options.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Set ERC options.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: irc local

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
