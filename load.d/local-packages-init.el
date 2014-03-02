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
;; Loading and configuring packages using `req-package`

;;; Code:

(require 'req-package)

;; Simple package declarations which don't require config.

(req-package apache-mode :ensure apache-mode)
(req-package auctex :ensure auctex)
(req-package auto-complete :ensure auto-complete)
(req-package bind-key :ensure bind-key)
(req-package coffee-mode :ensure coffee-mode)
(req-package csv-mode :ensure csv-mode)
(req-package diminish :ensure diminish)
(req-package flycheck :ensure flycheck)
(req-package helm-descbinds :ensure helm-descbinds)
(req-package iedit :ensure iedit)
(req-package inflections :ensure inflections)
(req-package info+ :ensure info+)
(req-package jade-mode :ensure jade-mode)
(req-package keyfreq :ensure keyfreq)
(req-package keywiz :ensure keywiz)
;;(req-package less :ensure less)
(req-package lua-mode :ensure lua-mode)
(req-package markdown-mode :ensure markdown-mode)
(req-package mediawiki :ensure mediawiki)
(req-package php-mode :ensure php-mode)
(req-package restclient :ensure restclient)
(req-package rust-mode :ensure rust-mode)
(req-package smooth-scrolling :ensure smooth-scrolling)
(req-package ssh-config-mode :ensure ssh-config-mode)
(req-package sws-mode :ensure sws-mode)
(req-package tidy :ensure tidy)
(req-package unbound :ensure unbound)
(req-package undo-tree :ensure undo-tree)
(req-package vlf :ensure vlf)
(req-package websocket :ensure websocket)
(req-package whitespace-cleanup-mode :ensure whitespace-cleanup-mode)
(req-package yaml-mode :ensure yaml-mode)
(req-package groovy-mode :ensure groovy-mode)
(req-package auto-indent-mode :ensure auto-indent-mode)

;; Complex package declarations

(req-package dired-details
  :ensure dired-details
  :init (autoload 'dired-details-install "dired-details")
  :config (add-hook 'after-init-hook 'dired-details-install))

(req-package elpy
  :require jedi
  :ensure elpy
  :config
  (progn
    (add-to-list 'exec-path (expand-file-name "~/.local/bin"))
    (elpy-use-ipython)
    (define-key elpy-mode-map [remap elpy-goto-definition] 'helm-semantic-or-imenu)))

(req-package erc
  :config
  (progn
    (add-hook 'erc-mode-hook 'visual-line-mode)

    (ad-activate 'erc-process-away)
    (ad-activate 'erc-cmd-AWAY)))

(req-package ess
  :ensure ess
  :config (require 'ess-site nil t))

(req-package geben
  :ensure geben
  :config
  (defadvice geben-dbgp-redirect-stream (around
                                         geben-output-inhibit-read-only
                                         activate)
    "Set `inhibit-read-only' during `geben-dbgp-redirect-stream'"
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      ad-do-it)
    (set-buffer-modified-p nil)))

(req-package geiser
  :require quack
  :ensure geiser
  :config
  (eval-after-load 'geiser-mode
    '(define-key geiser-mode-map [remap geiser-edit-symbol-at-point]
       'helm-semantic-or-imenu)))

(req-package graphviz-dot-mode
  :ensure graphviz-dot-mode
  :mode "\\.dot$")

(req-package helm
  :ensure helm
  :bind (("M-C-y" . helm-show-kill-ring)
         ("C-x f" . helm-recentf)
         ("C-x b" . helm-buffers-list))
  :config
  (progn
    (define-key esc-map [remap find-tag] 'helm-semantic-or-imenu)
    (global-set-key [remap find-tag] 'helm-semantic-or-imenu)
    (define-key help-map [remap apropos-command] 'helm-apropos)
    (global-set-key [remap apropos-command] 'helm-apropos)
    (when (boundp 'ido-minor-mode-map-entry)
      (define-key (cdr ido-minor-mode-map-entry)
        [remap ido-switch-buffer]
        'helm-buffers-list))))

;; Hopefully takes care of all those "Invalid face reference: helm-ff-directory"
;; errors.
;; (eval-after-load 'helm-buffers
;;   '(progn
;;      (require 'helm-files)))


(req-package hl-line+
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

(req-package js2-mode
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

(req-package magit
  :ensure magit
  :bind ("C-c g" . magit-status)
  :config
  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))

(req-package multiple-cursors
  :ensure multiple-cursors
  :bind (("C-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("s-SPC" . set-rectangular-region-anchor))
  :config (setq mc/list-file (concat tmp-dir ".mc-lists.el")))

(req-package org
  :require org-plus-contrib
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

(req-package paredit
  :ensure paredit
  :config
  (define-key lisp-interaction-mode-map (kbd "C-c C-e") 'eval-print-last-sexp))

(req-package ruby-mode
  :require (yard-mode ruby-block ruby-test-mode rvm)
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

(req-package inf-ruby
  :config (setf (car inf-ruby-implementations) '("ruby" . "pry")))

(req-package rinari
  :require (haml-mode feature-mode rspec-mode sass-mode less-css-mode)
  :ensure rinari)

(req-package scheme
  :mode (("\\.ss\\'" . scheme-mode)
         ("\\.scm$" . scheme-mode))
  :config (add-hook 'scheme-mode-hook 'paredit-mode))

(req-package smart-mode-line
  :ensure smart-mode-line
  :init (sml/setup))

(req-package smex
  :ensure smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ;; This is your old M-x.
         ("C-c M-x" . execute-extended-command))
  :config (smex-initialize))

(req-package pcache
  :ensure pcache
  :config (setq pcache-directory (concat tmp-dir "pcache")))

;; Pseudo-packages. Not actually elpa packages, but make use of `req-package'
;; for setup.

(defun dired-launch-command ()
  "Open the file at point."
  (interactive)
  (org-open-file (dired-get-filename)))

(req-package dired
  :config (progn
            (require 'org) ;; for `org-open-file'
            (define-key dired-mode-map "r" 'dired-launch-command)))

(req-package woman
  :config
  (progn
    ;;(add-hook 'woman-mode-hook 'less-minor-mode)
    (add-hook 'woman-mode-hook 'scroll-lock-mode)))

(req-package man
  :config
  (progn
    ;;(add-hook 'Man-mode-hook 'less-minor-mode)
    (add-hook 'Man-mode-hook 'scroll-lock-mode)))

(req-package conf-mode
  :mode "\.cnf$")

(req-package tramp
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

(use-package misc-keys
  ;; Set C-w to backward kill word and remap existing C-w to C-x C-k
  :bind (("C-w" . backward-kill-word)
         ("C-x C-k" . kill-region)
         ;; Use C-c k for kmacro keys
         ("C-c k" . kmacro-keymap)
         ("C-x C-o" . delete-blank-lines)
         ("M-/" . hippie-expand)
         ("C-x O" . other-window-backwards)
         ("C-x C-e" . eval-and-replace)))

(provide 'local-packages-init)

;;; local-packages-init.el ends here
