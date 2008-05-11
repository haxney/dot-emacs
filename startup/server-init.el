;; ---- Emacsclient
;; Only start when the server is not already started.
(when (or (not (boundp 'server-process))
          (not (eq (process-status server-process)
                   'listen)))
  (server-start))
