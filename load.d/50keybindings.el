;; Org-mode keys
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c M-d") 'org-open-day-page)
(global-set-key (kbd "C-c C-x C-o") 'my/org-clock-out)
(global-set-key (kbd "C-c r") 'org-capture)

;; Smex, a Super M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Set C-w to backward kill word and remap existing C-w to C-x C-k
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Use C-c k for kmacro keys
(global-set-key (kbd "C-c k") 'kmacro-keymap)

;; Use super with arrow keys for windmove.
(windmove-default-keybindings 'super)

(global-set-key (kbd "C-M-;") 'comment-dwim)

;; Paredit steals C-j from `eval-print-last-sexp'. Bring it back!
(define-key lisp-interaction-mode-map (kbd "C-c C-e") 'eval-print-last-sexp)
(global-set-key (kbd "C-x C-o") 'delete-blank-lines)
(global-set-key (kbd "C-x g") 'magit-status)

;; Make paredit play nice with cua's rectangle editing.
(eval-after-load "cua-rect"
  '(progn
     (define-key cua--rectangle-keymap [remap paredit-forward-delete] 'cua-delete-char-rectangle)
     (define-key cua--rectangle-keymap [remap paredit-backward-delete] 'cua-delete-char-rectangle)))

(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)))

(global-set-key (kbd "M-C-y") 'anything-show-kill-ring)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x f") 'esk-recentf-ido-find-file)

(defun esk-recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
