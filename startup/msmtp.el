;; we substitute sendmail with msmtp
(setq sendmail-program "/usr/bin/msmtp")

;;need to tell msmtp which account we're using
(setq message-sendmail-extra-arguments '("-a" "haxney"))

;; you might want to set the following too
(setq mail-host-address "gmail.com")
(setq user-full-name "Daniel Hackney")
(setq user-mail-address "dan@haxney.org")
