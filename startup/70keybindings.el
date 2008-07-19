;;; 70keybindings.el --- Set up keybindings.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Set up keybindings.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: convenience keybindings local

;; This file is NOT part of GNU Emacs.

;;; Code:

;; Set C-w to backward kill word and remap existing C-w to C-x C-k
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

;; Use C-c C-k for kmacro keys
(global-set-key "\C-c\C-k" 'kmacro-keymap)

;; Globally set C-c C-v C-c to compile.
(global-set-key "\C-c\C-v\C-c" 'compile)

;; Set F5 to replay last macro
(global-set-key [f5] 'call-last-kbd-macro)

;; Switching to speedbar.
(global-set-key "\C-co" 'speedbar-get-focus)

;; Commenting
(global-set-key (kbd "C-M-;") 'comment-region)

;; Use meta with arrow keys for windmove.
(windmove-default-keybindings 'super)

(define-key global-map "\C-cr" 'remember)

;;; 70keybindings.el ends here
