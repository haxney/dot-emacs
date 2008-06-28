;; Only start when the server is not already started.
(when (or (not (boundp 'server-process))
          (not (eq (process-status server-process)
                   'listen)))
  (server-start))

(defun server-perspective-switch ()
  "Open all server buffers in the \"server\" perspective.

This is intended to be a server hook."
  (let ((new-buf (switch-to-buffer (buffer-name)))
        (old-persp persp-curr-name))
    (persp-switch "server")
    (switch-to-buffer new-buf)
    (persp-switch old-persp)

    ;; Don't kill the buffer if we are already in the "server" perspective.
    (when (not (string-equal old-persp "server"))
      (persp-remove-buffer new-buf))
    (persp-switch "server")))


(defun server-edit-presets ()
  ;; When editing mail, set the goal-column to 72.
  (cond ((string-match "mail\.google\.com\.[0-9a-z]+\.txt" (buffer-name))
         (set-fill-column 72))
        ((string-match "www\.facebook\.com\.[0-9a-z]+\.txt" (buffer-name))
         (long-lines-mode))))

(add-hook 'server-switch-hook 'server-perspective-switch)

(add-hook 'server-visit-hook 'server-edit-presets)

