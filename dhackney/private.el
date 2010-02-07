;; Load the private files.
(if (file-readable-p "~/.private/private.el")
    (load-file "~/.private/private.el"))
