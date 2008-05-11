(setq gnus-select-method '(nnmaildir "local"
                                     (directory "~/Mail")))

;; Use cool thread visualization.
(setq gnus-sum-thread-tree-root "\x4912f "
      gnus-sum-thread-tree-single-indent "\x4912e "
      gnus-sum-thread-tree-leaf-with-other "\x4903c\x49020\x490fa "
      gnus-sum-thread-tree-vertical "\x49022"
      gnus-sum-thread-tree-single-leaf "\x490b0\x49020\x490fa ")

;; Fill article lines
(setq gnus-treat-fill-long-lines t)

;; Send mail with msmtp via smtpmail
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp"
      message-sendmail-extra-arguments '("-a" "haxney"))

