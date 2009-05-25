;;; 20autoload.el --- Set up autoloads for libaries.

;; Copyright (C) 2009 Daniel Hackney

;; Description: Set up autoloads for libaries.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: autoload lisp local

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  For libraries which are autoloaded without any additional configuration.

;;; Code:

(autoload 'scheme-mode "quack" "Enter scheme-mode." t)

(autoload 'git-status "git" "Enter git-status mode" t)

(autoload 'graphviz-dot-mode "graphviz-dot-mode" "Enter graphviz-dot-mode." t)
(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))

(autoload 'gri-mode "gri-mode" "Enter Gri-mode." t)

(autoload 'pgg-invoke "pgg" "Use a PGG command")

(autoload 'plan "planner" "Planner mode" t)

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

(autoload 'flymake-find-file-hook "flymake" "On-the-fly syntax checker." t)
(autoload 'flymake-mode "flymake" "On-the-fly syntax checker." t)

(autoload 'yaml-mode "yaml-mode" "Major mode for editing YAML files." t)

(autoload 'msf-abbrev-load "msf-abbrev"
  "Load all abbrevs under `msf-abbrev-root'.")

(autoload 'highline-mode "highline"
  "Highline line highlighting mode.")

(autoload 'global-linum-mode "linum"
  "Set linum mode globally.")

(require 'smart-tab)

;;; 20autoload.el ends here
