;;; local-erc.el --- Set ERC options.

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

;; Respond once if mentioned while away
(defvar erc-responded-once nil)
(defvar erc-away-reason nil)

(defun erc-respond-once-if-away (match-type nickuserhost msg)
  "Respond once to messages sent while away."
  (if (erc-away-time)
      (if (eq match-type 'current-nick)
          (unless erc-responded-once
            (erc-send-action (erc-default-target) (concat "is away: " erc-away-reason))
            (setq erc-responded-once t)))))

(defun erc-custom-prompt ()
  "A nicer default prompt."
  (if (and (boundp 'erc-default-recipients) (erc-default-target))
      (erc-propertize (concat (erc-default-target) ">")
                      'read-only t 'rear-nonsticky t 'front-nonsticky t)
    (erc-propertize "ERC>"
                    'read-only t 'rear-nonsticky t 'front-nonsticky t)))

(defun erc-generate-log-file-name-date-and-name (buffer target nick server port)
  "Generates a log-file name with the date and other info.

This results in a file name of the form \"2009-06-03-#channel@server:port.txt\".
This function is a possible value for `erc-generate-log-file-name-function'."
  (let ((file (concat
               (format-time-string "%Y-%m-%d")
               "-" target
               "@" server ".txt")))
    ;; we need a make-safe-file-name function.
    (convert-standard-filename file)))

(defadvice erc-process-away (after erc-away-reason-clear (proc away-p))
  "Clear things"
  (unless away-p
    (setq erc-responded-once nil
          erc-away-reason nil)))

(defadvice erc-cmd-AWAY (after erc-store-reason (line))
  "store line"
  (when (string-match "^\\s-*\\(.*\\)$" line)
    (let ((reason (match-string 1 line)))
      (setq erc-away-reason reason))))

(defun erc-cmd-DEMOGRAPHICS (&rest ignore)
  "Display short channel demographics."
  (erc-display-message nil 'notice (current-buffer)
                       (let ((hash-table erc-channel-users)
                             (demo-hashes (make-hash-table :test 'equal))
                             (tld-distri))
                         (maphash (lambda (k v)
                                    (let* ((host (format "%s" (aref (car v) 2)))
                                           (tld (and (string-match ".+\\.\\([a-zA-Z]+\\)$" host)
                                                     (downcase (match-string 1 host))))
                                           (tldc (and tld (gethash tld demo-hashes 0))))
                                      (and tldc (puthash tld (1+ tldc) demo-hashes))))
                                  hash-table)
                         (maphash (lambda (k v)
                                    (add-to-list 'tld-distri (format "%s %s" v k)))
                                  demo-hashes)
                         (format "TLD Demographics: %s" (mapconcat 'identity tld-distri ", ")))))
(defalias 'erc-cmd-DG 'erc-cmd-DEMOGRAPHICS)

(defun erc-cmd-HOWMANY (&rest ignore)
  "Display how many users (and ops) the current channel has."
  (erc-display-message nil 'notice (current-buffer)
                       (let ((hash-table (with-current-buffer
                                             (erc-server-buffer)
                                           erc-server-users))
                             (users 0)
                             (ops 0))
                         (maphash (lambda (k v)
                                    (when (member (current-buffer)
                                                  (erc-server-user-buffers v))
                                      (incf users))
                                    (when (erc-channel-user-op-p k)
                                      (incf ops)))
                                  hash-table)
                         (format
                          "There are %s users (%s ops) on the current channel"
                          users ops))))

(defun bitlbee ()
  "Connect to local bitlbee server."
  (interactive)
  (erc :server "localhost"))

(provide 'local-erc)

;;; local-erc.el ends here
