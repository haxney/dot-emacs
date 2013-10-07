;;; local-keybindings.el --- Keybindings for me.

;; Org-mode keys
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c M-d") 'org-open-day-page)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "C-c r") 'org-capture)

;; Smex, a Super M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; This is your old M-x.
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

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
(global-set-key (kbd "C-c g") 'magit-status)

;; Make paredit play nice with cua's rectangle editing.
(eval-after-load "cua-rect"
  '(progn
     (define-key cua--rectangle-keymap [remap paredit-forward-delete] 'cua-delete-char-rectangle)
     (define-key cua--rectangle-keymap [remap paredit-backward-delete] 'cua-delete-char-rectangle)))

(global-set-key (kbd "M-C-y") 'helm-show-kill-ring)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x f") 'helm-recentf)

(define-key esc-map [remap find-tag] 'helm-semantic-or-imenu)
(global-set-key [remap find-tag] 'helm-semantic-or-imenu)
;; Hopefully takes care of all those "Invalid face reference: helm-ff-directory"
;; errors.
(eval-after-load 'helm-buffers
  '(progn
     (require 'helm-files)))

(define-key help-map [remap apropos-command] 'helm-apropos)
(global-set-key [remap apropos-command] 'helm-apropos)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(when (boundp 'ido-minor-mode-map-entry)
  (define-key (cdr ido-minor-mode-map-entry)
    [remap ido-switch-buffer]
    'helm-buffers-list))

(eval-after-load 'comint
  '(defadvice comint-previous-input
     (around restore-comint-input-with-zero-prefix activate)
    "Make `comint-previous-input' restore the input with arg == 0"
    (if (and
         comint-input-ring-index
         comint-stored-incomplete-input
         (eq arg 0))
        (comint-restore-input)
      ad-do-it)))

(eval-after-load 'geiser-mode
  '(progn
     (define-key geiser-mode-map [remap geiser-edit-symbol-at-point] 'helm-semantic-or-imenu)))

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(eval-after-load 'magit
  '(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value.

From https://github.com/magnars/.emacs.d"
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-x C-e") 'eval-and-replace)

(eval-after-load 'info
  '(progn
     (define-key Info-mode-map (kbd ";") 'Info-next-reference)
     (define-key Info-mode-map (kbd "'") 'Info-prev-reference)))

;; Multiple-cursors
(global-set-key (kbd "C-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "s-SPC") 'set-rectangular-region-anchor)

(eval-after-load 'info
  '(progn
     (define-key Info-mode-map (kbd ";") 'Info-next-reference)
     (define-key Info-mode-map (kbd "'") 'Info-prev-reference)))

;;; local-keybindings.el ends here
