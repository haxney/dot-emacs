;; Only start when the server is not already started.
(when (or (not (boundp 'server-process))
          (not (eq (process-status server-process)
                   'listen)))
  (server-start))

;; When editing mail, set the goal-column to 72.
(add-hook 'server-visit-hook
          (lambda ()
            (when (string-match "mail\.google\.com\.[0-9a-z]+\.txt" (buffer-name))
              (set-fill-column 72))))
