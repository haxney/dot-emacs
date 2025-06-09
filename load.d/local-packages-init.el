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

;; Simple package declarations which don't require config.

(use-package gh)
(use-package haml-mode)
(use-package feature-mode)
(use-package rspec-mode)
(use-package sass-mode)
(use-package less-css-mode)
(use-package apache-mode)
(use-package auto-complete)
(use-package bind-key)
(use-package coffee-mode)
(use-package csv-mode)
(use-package diminish)
(use-package flycheck)
(use-package helm-descbinds)
(use-package iedit)
(use-package inflections)
(use-package jade-mode)
(use-package keyfreq)
(use-package keywiz)
(use-package lua-mode)
(use-package markdown-mode)
(use-package mediawiki)
(use-package restclient)
(use-package rust-mode
  :config
  (progn
    (add-hook 'rust-mode-hook '(lambda () (set-fill-column 100)))))
(use-package smooth-scrolling)
(use-package sws-mode)
(use-package tidy)
(use-package unbound)
(use-package undo-tree)
(use-package vlf)
(use-package websocket)
(use-package whitespace-cleanup-mode)
(use-package yaml-mode)
(use-package jedi)
(use-package quack)
(use-package toml-mode)
(use-package racer)
(use-package cargo)
(use-package protobuf-mode)
(use-package clang-format)

(use-package ccls
  ;; :hook ((c-mode c++-mode objc-mode cuda-mode) .
  ;;        (lambda () (require 'ccls) (lsp)))
  )
(use-package lsp-mode :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)


;; Complex package declarations

(use-package ace-jump-mode
  :bind (("C-c j" . ace-jump-mode)))

(use-package auto-indent-mode
  :config
  (progn
    (advice-remove 'beginning-of-visual-line
                   #'ad-Advice-move-beginning-of-line)))

(use-package dired-details
  :init (autoload 'dired-details-install "dired-details")
  :config (add-hook 'after-init-hook 'dired-details-install))

(use-package elpy
  :config
  (progn
    (add-to-list 'exec-path (expand-file-name "~/.local/bin"))
    (define-key elpy-mode-map [remap elpy-goto-definition] 'helm-semantic-or-imenu)))

(use-package erc
  :config
  (progn
    (add-hook 'erc-mode-hook 'visual-line-mode)

    (ad-activate 'erc-process-away)
    (ad-activate 'erc-cmd-AWAY)))

(use-package ess
  :config (require 'ess-site nil t))

(use-package geben
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
  :config
  (eval-after-load 'geiser-mode
    '(define-key geiser-mode-map [remap geiser-edit-symbol-at-point]
       'helm-semantic-or-imenu)))

(use-package graphviz-dot-mode)

(use-package helm
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
  :bind ("C-c g" . magit-status)
  :config
  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))

(use-package multiple-cursors
  :bind (("C-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("s-SPC" . set-rectangular-region-anchor))
  :config (setq mc/list-file (concat tmp-dir ".mc-lists.el")))

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c M-d" . org-open-day-page)
         ("C-c C-x C-o" . org-clock-out)
         ("C-c r" . org-capture)
         :map dired-mode-map
         ("r" . 'my/dired-launch-command))
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

    (org-add-link-type "tel" nil 'org-format-export-tel-link)))


(use-package paredit
  :bind (:map lisp-interaction-mode-map
              ("C-c C-e" . eval-print-last-sexp)
              :map paredit-mode-map
              ("<return>" . my/paredit-RET))
  :config
  (defun my/paredit-RET ()
    "Wraps `paredit-RET' to provide a sensible minibuffer experience."
    (interactive)
    (cond
     ((minibufferp)
      (read--expression-try-read))
     ((and (eq major-mode 'inferior-emacs-lisp-mode)
           (string-prefix-p "*ielm*" (buffer-name)))
      (ielm-return))
     (t
      (paredit-RET)))))



(use-package platformio-mode
  :config
  (progn
    (add-to-list 'exec-path (expand-file-name "~/.platformio/penv/bin") t)
    (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.platformio/penv/bin")))
    (add-hook 'c++-mode-hook 'platformio-conditionally-enable)

    (require 'ansi-color)
    (defun endless/colorize-compilation ()
      "Colorize from `compilation-filter-start' to `point'."
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region
         compilation-filter-start (point))))

    (add-hook 'compilation-filter-hook
              #'endless/colorize-compilation)))

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

(use-package yard-mode)
(use-package ruby-test-mode)
(use-package rvm)

(use-package scheme
  :mode (("\\.ss\\'" . scheme-mode)
         ("\\.scm$" . scheme-mode))
  :config (add-hook 'scheme-mode-hook 'paredit-mode))

(use-package smart-mode-line
  :after (my-custom-values)
  :init (sml/setup))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ;; This is your old M-x.
         ("C-c M-x" . execute-extended-command))
  :config (smex-initialize))

(use-package pcache
  :config (setq pcache-directory (concat tmp-dir "pcache")))

;; Pseudo-packages. Not actually elpa packages, but make use of `use-package'
;; for setup.

(defun my/dired-launch-command ()
  "Open the file at point."
  (interactive)
  (org-open-file (dired-get-filename)))

(use-package dired
  :config (progn
            (require 'org) ;; for `org-open-file'
            (define-key dired-mode-map "r" 'my/dired-launch-command)))

(use-package woman
  :config
  (progn
    (add-hook 'woman-mode-hook 'scroll-lock-mode)))

(use-package man
  :config
  (progn
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

(use-package editorconfig
  :config
  (editorconfig-mode 1))

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
