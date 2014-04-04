;;; local-packages-init.el --- Initialization of packages

;; Copyright (C) 2014 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: convenience files local

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
;; Loading and configuring packages using `use-package`

;;; Code:

(require 'use-package)

;; Simple package declarations which don't require config.

(use-package gh :ensure t)
(use-package rinari :ensure rinari)
(use-package haml-mode :ensure t)
(use-package feature-mode :ensure t)
(use-package rspec-mode :ensure t)
(use-package sass-mode :ensure t)
(use-package less-css-mode :ensure t)
(use-package apache-mode :ensure apache-mode)
;;(use-package auctex :ensure t :defer t)
(use-package auto-complete :ensure auto-complete)
(use-package bind-key :ensure bind-key)
(use-package coffee-mode :ensure coffee-mode)
(use-package csv-mode :ensure csv-mode)
(use-package diminish :ensure diminish)
(use-package flycheck :ensure flycheck)
(use-package helm-descbinds :ensure helm-descbinds)
(use-package iedit :ensure iedit)
(use-package inflections :ensure inflections)
(use-package info+ :ensure info+)
(use-package jade-mode :ensure jade-mode)
(use-package keyfreq :ensure keyfreq)
(use-package keywiz :ensure keywiz)
;;(use-package less :ensure less)
(use-package lua-mode :ensure lua-mode)
(use-package markdown-mode :ensure markdown-mode)
(use-package mediawiki :ensure mediawiki)
(use-package php-mode :ensure php-mode)
(use-package restclient :ensure restclient)
(use-package rust-mode :ensure rust-mode)
(use-package smooth-scrolling :ensure smooth-scrolling)
(use-package ssh-config-mode :ensure ssh-config-mode)
(use-package sws-mode :ensure sws-mode)
(use-package tidy :ensure tidy)
(use-package unbound :ensure unbound)
(use-package undo-tree :ensure undo-tree)
(use-package vlf :ensure vlf)
(use-package websocket :ensure websocket)
(use-package whitespace-cleanup-mode :ensure whitespace-cleanup-mode)
(use-package yaml-mode :ensure yaml-mode)
(use-package groovy-mode :ensure groovy-mode)
(use-package auto-indent-mode :ensure auto-indent-mode)
(use-package jedi :ensure t)
(use-package quack :ensure t)

;; Complex package declarations

(use-package dired-details
  :ensure dired-details
  :init (autoload 'dired-details-install "dired-details")
  :config (add-hook 'after-init-hook 'dired-details-install))

(use-package elpy
  :ensure elpy
  :config
  (progn
    (add-to-list 'exec-path (expand-file-name "~/.local/bin"))
    (elpy-use-ipython)
    (define-key elpy-mode-map [remap elpy-goto-definition] 'helm-semantic-or-imenu)))

(use-package erc
  :defer t
  :config
  (progn
    (add-hook 'erc-mode-hook 'visual-line-mode)

    (ad-activate 'erc-process-away)
    (ad-activate 'erc-cmd-AWAY)))

(use-package ess
  :ensure ess
  :defer t
  :config (require 'ess-site nil t))

(use-package geben
  :ensure geben
  :defer t
  :config
  (defadvice geben-dbgp-redirect-stream (around
					 geben-output-inhibit-read-only
					 activate)
    "Set `inhibit-read-only' during `geben-dbgp-redirect-stream'"
    (let ((inhibit-read-only t)
	  (inhibit-modification-hooks t))
      ad-do-it)
    (set-buffer-modified-p nil)))

(use-package geiser
  :ensure geiser
  :config
  (eval-after-load 'geiser-mode
    '(define-key geiser-mode-map [remap geiser-edit-symbol-at-point]
       'helm-semantic-or-imenu)))

(use-package graphviz-dot-mode
  :ensure graphviz-dot-mode
  :mode "\\.dot$")

(use-package helm
  :ensure helm
  :config
  (progn
    (bind-key "M-C-y" 'helm-show-kill-ring)
    (bind-key "C-x f" 'helm-recentf)
    (bind-key "C-x b" 'helm-buffers-list)
    (define-key esc-map [remap find-tag] 'helm-semantic-or-imenu)
    (global-set-key [remap find-tag] 'helm-semantic-or-imenu)
    (define-key help-map [remap apropos-command] 'helm-apropos)
    (global-set-key [remap apropos-command] 'helm-apropos)
    (when (and (boundp 'ido-minor-mode-map-entry)
	       ido-minor-mode-map-entry)
      (define-key (cdr ido-minor-mode-map-entry)
	[remap ido-switch-buffer]
	'helm-buffers-list))
    ;; Hopefully takes care of all those "Invalid face reference:
    ;; helm-ff-directory" errors.
    (eval-after-load 'helm-buffers
      '(progn
	 (require 'helm-files)))))

(use-package hl-line+
  :ensure hl-line+
  :config
  (progn
    (defvar hl-line-ignore-regexp "\*magit:.*")
    (defadvice global-hl-line-highlight (around unhighlight-some-buffers
						()
						activate)
      "Don't highlight in buffers which match a regexp."
      (unless (string-match hl-line-ignore-regexp
			    (buffer-name (window-buffer (selected-window))))
	ad-do-it))))

(use-package js2-mode
  :ensure js2-mode
  :mode (("\\.js\\'" . js2-mode)
	 ("\\.json\\'" . javascript-mode)))

(defun magit-toggle-whitespace ()
  "Toggle whitespace."
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  "Ignore whitespace."
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  "Don't ignore whitespace."
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(use-package magit
  :ensure magit
  :bind ("C-c g" . magit-status)
  :config
  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))

(use-package multiple-cursors
  :ensure multiple-cursors
  :bind (("C-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)
	 ("s-SPC" . set-rectangular-region-anchor))
  :config (setq mc/list-file (concat tmp-dir ".mc-lists.el")))

(use-package org
  :ensure org
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c M-d" . org-open-day-page)
	 ("C-c C-x C-o" . org-clock-out)
	 ("C-c r" . org-capture))
  :config
  (progn
    (defun org-open-day-page ()
      "Prompt for a date, and open associated day-page."
      (interactive)
      (find-file (expand-file-name
		  (concat (replace-regexp-in-string "-" "." (org-read-date nil))
			  ".org")
		  org-directory)))

    (defun org-format-export-tel-link (path desc format)
      "Format a tel: link for export"
      (case format
	(html
	 (format "<a href=\"%s\">%s</a>" path desc))
	(latex
	 (format "\\href{tel:%s}{\\texttt{%s}}" path desc))))

    (define-key org-mode-map (kbd "C-M-m") 'org-insert-heading-after-current)
    (org-add-link-type "tel" nil 'org-format-export-tel-link)))

(use-package paredit
  :ensure paredit
  :config
  (define-key lisp-interaction-mode-map (kbd "C-c C-e") 'eval-print-last-sexp))

(use-package ruby-mode
  :mode (("Gemfile$" . ruby-mode)
	 ("Buildfile$" . ruby-mode)
	 ("config.ru$" . ruby-mode)
	 ("\\.rake$" . ruby-mode)
	 ("Rakefile$" . ruby-mode)
	 ("\\.rabl$" . ruby-mode)
	 ("\\.json_builder$" . ruby-mode))
  :config
  (progn
    (add-hook 'ruby-mode-hook 'flyspell-prog-mode)
    (add-hook 'ruby-mode-hook 'yard-mode)))

(use-package yard-mode :ensure t)
(use-package ruby-test-mode :ensure t)
(use-package rvm :ensure t)

(use-package inf-ruby
  :config (setf (car inf-ruby-implementations) '("ruby" . "pry")))

(use-package scheme
  :mode (("\\.ss\\'" . scheme-mode)
	 ("\\.scm$" . scheme-mode))
  :config (add-hook 'scheme-mode-hook 'paredit-mode))

(use-package smart-mode-line
  :ensure smart-mode-line
  :init (sml/setup))

(use-package smex
  :ensure smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ;; This is your old M-x.
	 ("C-c M-x" . execute-extended-command))
  :config (smex-initialize))

(use-package pcache
  :ensure pcache
  :config (setq pcache-directory (concat tmp-dir "pcache")))

;; Pseudo-packages. Not actually elpa packages, but make use of `use-package'
;; for setup.

(defun dired-launch-command ()
  "Open the file at point."
  (interactive)
  (org-open-file (dired-get-filename)))

(use-package dired
  :config (progn
	    (require 'org) ;; for `org-open-file'
	    (define-key dired-mode-map "r" 'dired-launch-command)))

(use-package woman
  :config
  (progn
    ;;(add-hook 'woman-mode-hook 'less-minor-mode)
    (add-hook 'woman-mode-hook 'scroll-lock-mode)))

(use-package man
  :config
  (progn
    ;;(add-hook 'Man-mode-hook 'less-minor-mode)
    (add-hook 'Man-mode-hook 'scroll-lock-mode)))

(use-package conf-mode
  :mode "\.cnf$")

(use-package tramp
  :config
  (progn
    ;; Allow "/sudo:host:/etc/stuff" to sudo on a remote host
    (add-to-list 'tramp-default-proxies-alist
		 '(nil "\\`root\\'" "/ssh:%h:"))
    (add-to-list 'tramp-default-proxies-alist
		 '((regexp-quote (system-name)) nil nil))))

(use-package abbrev
  :diminish abbrev-mode)

(use-package newcomment
  :bind ("C-M-;" . comment-dwim))

(use-package cua-base
  :config
  (progn
    ;; Make paredit play nice with cua's rectangle editing.
    (define-key cua--rectangle-keymap [remap paredit-forward-delete] 'cua-delete-char-rectangle)
    (define-key cua--rectangle-keymap [remap paredit-backward-delete] 'cua-delete-char-rectangle)))

(use-package comint
  :config
  '(defadvice comint-previous-input
     (around restore-comint-input-with-zero-prefix activate)
     "Make `comint-previous-input' restore the input with arg == 0"
     (if (and
	  comint-input-ring-index
	  comint-stored-incomplete-input
	  (eq arg 0))
	 (comint-restore-input)
       ad-do-it)))

(use-package info
  :config
  (progn
    (define-key Info-mode-map (kbd ";") 'Info-next-reference)
    (define-key Info-mode-map (kbd "'") 'Info-prev-reference)))

(defun other-window-backwards ()
  "Move backwards one window."
  (interactive)
  (other-window -1))

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

;; Set C-w to backward kill word and remap existing C-w to C-x C-k
(bind-key "C-w" 'backward-kill-word)
(bind-key "C-x C-k" 'kill-region)
;; Use C-c k for kmacro keys
(bind-key "C-c k" 'kmacro-keymap)
(bind-key "C-x C-o" 'delete-blank-lines)
(bind-key "M-/" 'hippie-expand)
(bind-key "C-x O" 'other-window-backwards)
(bind-key "C-x C-e" 'eval-and-replace)

(provide 'local-packages-init)

;;; local-packages-init.el ends here
