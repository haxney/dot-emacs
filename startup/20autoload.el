;;; 10autoload.el --- Set up autoloads for libaries.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Set up autoloads for libaries.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: autoload lisp local

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  For libraries which are autoloaded without any additional configuration.

;;; Code:

;; ---- Quack
;; Provides Scheme mode
(autoload 'scheme-mode "quack" "Enter scheme-mode." t)
(add-to-list 'auto-mode-alist '("\\.ss$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))

;; ---- Git
(autoload 'git-status "git" "Enter git-status mode" t)

;; ---- Dot
(autoload 'graphviz-dot-mode "graphviz-dot-mode" "Enter graphviz-dot-mode." t)
(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))

;; ---- Gri-mode
(autoload 'gri-mode "gri-mode" "Enter Gri-mode." t)
(add-to-list 'auto-mode-alist '("\\.gri$" . gri-mode))

;; ---- PGG
;; Encryption functions.
(autoload 'pgg-invoke "pgg" "Use a PGG command")

;; ---- Planner
(autoload 'plan "planner" "Planner mode" t)

;; ---- Flyspell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;; ---- Flymake
(autoload 'flymake-find-file-hook "flymake" "On-the-fly syntax checker." t)
(autoload 'flymake-mode "flymake" "On-the-fly syntax checker." t)

;; ---- YAML mode
(autoload 'yaml-mode "yaml-mode" "Major mode for editing YAML files." t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; ---- Ditz mode
(autoload 'ditz-todo "ditz" "Ditz bug tracking system." t)

;; ---- MSF-Abbrev
(autoload 'msf-abbrev-load "msf-abbrev"
  "Load all abbrevs under `msf-abbrev-root'.")

;; ---- AUCTeX
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; ---- Highline
(autoload 'highline-mode "highline"
  "Highline line highlighting mode.")

(autoload 'global-linum-mode "linum"
  "Set linum mode globally.")

;; ---- Whitespace
(add-hook 'mail-send-hook 'delete-trailing-whitespace)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; ---- PHP MuMaMo mode
(add-to-list 'auto-mode-alist '("\\.module$" . nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . nxhtml-mumamo-mode))
(require 'smart-tab)

;;; 10autoload.el ends here
