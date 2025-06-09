;;; local-packages-init.el --- Initialization of packages -*- lexical-binding: t; -*-

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

;; Load this first to allow `use-package' to use the `:delight' keyword.
(use-package delight)

(use-package editorconfig :delight)
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
(use-package rust-mode)
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
(use-package ccls)
(use-package lsp-mode)
(use-package lsp-ui)
(use-package company-lsp)

;; Complex package declarations

(use-package snap-indent
  :hook (prog-mode . snap-indent-mode)
  :custom ((snap-indent-format 'untabify)
           (snap-indent-on-save t)))

(use-package elpy
  :bind (:map elpy-mode-map
              ([remap elpy-goto-definition] . helm-semantic-or-imenu))
  :config
  (progn
    (add-to-list 'exec-path (expand-file-name "~/.local/bin"))))

(use-package erc
  :hook ((erc-mode . visual-line-mode))
  :config
  (progn
    (ad-activate 'erc-process-away)
    (ad-activate 'erc-cmd-AWAY)))

(use-package ess)

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
  :defines geiser-mode-map
  :bind (:map geiser-mode-map
              ([remap geiser-edit-symbol-at-point] . helm-semantic-or-imenu)))

(use-package graphviz-dot-mode)

(use-package helm
  :bind (("M-C-y" . helm-show-kill-ring)
         ("C-x f" . helm-recentf)
         ("C-x b" . helm-buffers-list)
         ([remap find-tag] . helm-semantic-or-imenu)
         ([remap apropos-command] . helm-apropos)
         :map esc-map
         ([remap find-tag] . helm-semantic-or-imenu)
         :map help-map
         ([remap apropos-command] . helm-apropos))
  :config
  (progn
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

(use-package magit
  :bind (("C-c g" . magit-status)
         :map magit-status-mode-map
         ("W" . magit-toggle-whitespace))
  :defines (magit-diff-options)
  :commands (magit-refresh magit-ignore-whitespace magit-dont-ignore-whitespace)
  :config
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

  (defun magit-toggle-whitespace ()
    "Toggle whitespace."
    (interactive)
    (if (member "-w" magit-diff-options)
        (magit-dont-ignore-whitespace)
      (magit-ignore-whitespace))))

(use-package multiple-cursors
  :bind (("C-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("s-SPC" . set-rectangular-region-anchor)))

(use-package org
  :autoload (org-link-set-parameters org-read-date)
  :functions (dired-get-filename)
  :commands (my/dired-launch-command)
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
      (cl-case format
        (html
         (format "<a href=\"%s\">%s</a>" path desc))
        (latex
         (format "\\href{tel:%s}{\\texttt{%s}}" path desc))))

    (org-link-set-parameters "tel" :export 'org-format-export-tel-link)

    (defun my/dired-launch-command ()
      "Open the file at point."
      (interactive)
      (org-open-file (dired-get-filename)))))


(use-package paredit
  :bind (:map lisp-interaction-mode-map
              ("C-c C-e" . eval-print-last-sexp)
              :map paredit-mode-map
              ("<return>" . my/paredit-RET))
  :functions (ielm-return)
  :commands (paredit-RET)
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
  :hook ((c++-mode . platformio-conditionally-enable)
         (compilation-filter . endless/colorize-compilation))
  :functions (ansi-color-apply-on-region)
  :config
  (progn
    (add-to-list 'exec-path (expand-file-name "~/.platformio/penv/bin") t)
    (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.platformio/penv/bin")))

    (require 'ansi-color)
    (defun endless/colorize-compilation ()
      "Colorize from `compilation-filter-start' to `point'."
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region
         compilation-filter-start (point))))))

(use-package ruby-mode
  :hook ((ruby-mode . flyspell-prog-mode)
         (ruby-mode . yard-mode))
  :mode ("\\.json_builder\\'"))

(use-package yard-mode)
(use-package ruby-test-mode)
(use-package rvm)

(use-package scheme
  :hook (scheme-mode . paredit-mode)
  :mode (("\\.ss\\'" . scheme-mode)
         ("\\.scm\\'" . scheme-mode)))

(use-package smart-mode-line
  :config (sml/setup))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ;; This is your old M-x.
         ("C-c M-x" . execute-extended-command))
  :config (smex-initialize))

(use-package pcache)

(use-package woman
  :hook (woman-mode . scroll-lock-mode))

(use-package man
  :hook (Man-mode . scroll-lock-mode))

(use-package conf-mode
  :mode "\\.cnf\\'")

(use-package tramp
  :config
  (progn
    ;; Allow "/sudo:host:/etc/stuff" to sudo on a remote host
    (add-to-list 'tramp-default-proxies-alist
                 '(nil "\\`root\\'" "/ssh:%h:"))
    (add-to-list 'tramp-default-proxies-alist
                 '((regexp-quote (system-name)) nil nil))))

(use-package cua-base
  ;; Make paredit play nice with cua's rectangle editing.
  :bind (:map cua--rectangle-keymap
    ([remap paredit-forward-delete] . cua-delete-char-rectangle)
    ([remap paredit-backward-delete] . cua-delete-char-rectangle)))

(use-package info
  :bind (:map Info-mode-map
              (";" . Info-next-reference)
              ("'" . Info-prev-reference)))

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
