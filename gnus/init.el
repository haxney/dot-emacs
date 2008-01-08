;; Settings for gmail
(setq gnus-select-method '(nnimap "gmail"
           (nnimap-address "imap.gmail.com")
           (nnimap-server-port 993)
           (nnimap-stream ssl)))

;; Use topic-mode as default
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-invalid-group-regexp "[:`'\"]\\|^$")
