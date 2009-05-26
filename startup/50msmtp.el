;;; 50msmtp.el --- Settings for MSMTP mail sending.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: mail local

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
