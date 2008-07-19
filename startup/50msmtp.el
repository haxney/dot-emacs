;;; 50msmtp.el --- Settings for MSMTP mail sending.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Settings for MSMTP mail sending.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: mail local

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  Use MSMTP for mail sending.

;;; Code:

;; we substitute sendmail with msmtp
(setq sendmail-program "/usr/bin/msmtp")

;;need to tell msmtp which account we're using
(setq message-sendmail-extra-arguments '("-a" "haxney"))

;; you might want to set the following too
(setq mail-host-address "gmail.com")
(setq user-full-name "Daniel Hackney")
(setq user-mail-address "dan@haxney.org")

;;; 50msmtp.el ends here
