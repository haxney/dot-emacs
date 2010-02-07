;; Set C-w to backward kill word and remap existing C-w to C-x C-k
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Use C-c k for kmacro keys
(global-set-key (kbd "C-c k") 'kmacro-keymap)

;; Use super with arrow keys for windmove.
(windmove-default-keybindings 'super)

(global-set-key (kbd "C-c r") 'remember)

(global-set-key (kbd "C-c i u") 'identica-update-status-interactive)
(global-set-key (kbd "C-c i i") 'identica)

(define-key dired-mode-map (kbd "C-!")
  '(lambda () (interactive) (dired-do-shell-command "totem" nil (dired-get-marked-files t current-prefix-arg))))
