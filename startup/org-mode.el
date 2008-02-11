;; Org mode keys
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(eval-after-load 'org
  '(progn
     ;; Allow indentation without having to go to the arrow keys
     (define-key org-mode-map "\C-c\C-x\C-f" 'org-shiftmetaright)
     (define-key org-mode-map "\C-c\C-x\C-b" 'org-shiftmetaleft)
     (define-key org-mode-map "\C-\M-m" 'org-insert-heading-after-current)
     (define-key org-mode-map "\C-c\C-x\C-o" 'my/org-todo-waiting)
     (define-key org-mode-map "\C-c\C-x\C-i" 'my/org-todo-starting)

     ;; Turn on Flyspell when loading org-mode
     (add-hook 'org-mode-hook 'flyspell-mode)

     ;; Custom agenda commands
     (setq org-agenda-custom-commands
           '(("p" tags "PROJECT-MAYBE-DONE" nil)
             ("m" tags "PROJECT&MAYBE" nil)
             ))

     (defun my/org-todo-waiting ()
       (interactive)
       "Mark the current task WAITING."
       (org-clock-goto)
       (org-todo "WAITING")
       ;; This is necessary because org-clock-goto inists on opening in another window.
       (switch-to-buffer nil)
       (other-window -1))

     (defun my/org-todo-starting ()
       (interactive)
       "Mark the current task WAITING."
       (org-todo "STARTED"))

     ;; Add Sacha Chua's 'clock-in(out)-if-starting' functions
     (defun wicked/org-clock-in-if-starting ()
       "Clock in when the task is marked STARTED."
       (when (and (string= state "STARTED")
                  (not (string= last-state state)))
         (org-clock-in)))
      (add-hook 'org-after-todo-state-change-hook
                'wicked/org-clock-in-if-starting)
;;      (defadvice org-clock-in (after wicked activate)
;;        "Set this task's status to 'STARTED'."
;;        (org-todo "STARTED"))
     (defun wicked/org-clock-out-if-waiting ()
       "Clock out when the task is marked WAITING."
       (when (and (string= state "WAITING")
                  (equal (marker-buffer org-clock-marker) (current-buffer))
                  (< (point) org-clock-marker)
                  (> (save-excursion (outline-next-heading) (point))
                     org-clock-marker)
                  (not (string= last-state state)))
         (org-clock-out)))
     (add-hook 'org-after-todo-state-change-hook
               'wicked/org-clock-out-if-waiting)))
