;; Set C-w to backward kill word and remap existing C-w to C-x C-k
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

;; Globally set C-c C-v C-c to compile.
(global-set-key "\C-c\C-v\C-c" 'compile)

;; Set F5 to replay last macro
(global-set-key [f5] 'call-last-kbd-macro)

;; Switching to speedbar.
(global-set-key "\C-co" 'speedbar-get-focus)

;; Commenting
(global-set-key (kbd "C-M-;") 'comment-region)

;; Use meta with arrow keys for windmove.
(windmove-default-keybindings 'meta)

;; Use C-c C-k for kmacro keys
(global-set-key "\C-c\C-k" 'kmacro-keymap)
