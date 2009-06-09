;;; 09autoload.el --- Set up autoloads for libaries.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: autoload lisp local

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  For libraries which are autoloaded without any additional configuration.

;;; Code:

(autoload 'scheme-mode "quack" "Enter scheme-mode." t)

(autoload 'git-status "git" "Enter git-status mode" t)

(autoload 'graphviz-dot-mode "graphviz-dot-mode" "Enter graphviz-dot-mode." t)

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

(autoload 'erc-tls "erc"
  "Interactively select TLS connection parameters and run ERC.")

(autoload 'geben "geben"
  "Start GEBEN, a DBGp protocol frontend - a script debugger." t)

;; These two just do now want to autoload properly. The only option left is to
;; `require' them.
(require 'smart-tab)
(require 'auto-complete)

;;; 09autoload.el ends here
