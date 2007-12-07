;; Rebind M-x to C-x C-m
(global-set-key "\C-x\C-m" 'execute-extended-command)

;; Set C-w to backward kill word and remap existing C-w to C-x C-k
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

;; Globally set C-c C-v C-c to compile.
(global-set-key "\C-c\C-v\C-c" 'compile)

;; Set F5 to replay last macro
(global-set-key [f5] 'call-last-kbd-macro)

;; Switching to speedbar.
(global-set-key "\C-co" 'speedbar-get-focus)

;; For multi-tty, make C-x C-c kill only the current frame.
(global-set-key "\C-x\C-c" 'delete-frame)
