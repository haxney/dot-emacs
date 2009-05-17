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

     (require 'notify)
     (defun dhackney-notify-erc (match-type nickuserhost message)
       "Notify when a message is received"
       (notify (progn
                 (string-match "\\([^!]+\\)!" nickuserhost)
                 (match-string 1 nickuserhost))
               message
               :icon "emacs-snapshot"))

     (add-hook 'erc-text-matched-hook 'dhackney-notify-erc)))

(defun irc-maybe ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (erc :server "irc.freenode.net" :port 6667
         :nick "chrono325" :full-name "Daniel Hackney")))
;;; 50erc.el ends here
