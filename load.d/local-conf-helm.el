;;; local-conf-helm.el --- Set up helm.el

;; Copyright (C) 2009, 2012 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: convenience, local, matching

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
;; Provides some convenient functions for helm.el

;;; Code:

;; Variables which aren't (yet) set through `defcustom'
(setq helm-idle-delay 0.01
      helm-input-idle-delay 0.01
      helm-candidate-number-limit 50)

(defun local-conf-helm-descbinds-set (sym val)
  "The function for the `defcustom' `:set' keyword."
  (if val
      (progn
        (define-key help-map [remap describe-bindings] 'helm-descbinds)
        (global-set-key [remap describe-bindings] 'helm-descbinds))
    (define-key help-map [remap describe-bindings] nil)
    (global-unset-key [remap describe-bindings]))
  (set-default sym val))

(defgroup local-conf-helm nil
  "Local configuration for `helm'."
  :group 'local-conf
  :prefix "local-conf-helm-")

(defcustom local-conf-helm-descbinds-active nil
  "Whether `helm-descbinds' is on.
If setting manually, the remapped bindings for
`describe-bindings' must be set or unset. It is easiest to call
`local-conf-helm-descbinds-set' like

  (local-conf-helm-descbinds-set 'local-conf-helm-descbinds-active newval)

to set `local-conf-helm-descbinds-active' to `newval'."
  :type 'boolean
  :group 'local-conf-helm
  :group 'helm-descbinds
  :require 'helm-descbinds
  :set 'local-conf-helm-descbinds-set)


(autoload 'helm-descbinds "helm-descbinds" nil t)
(require 'helm-match-plugin)

(require 'helm-help)
(defun helm-c-buffer-list ()
  "Return a list of buffer names.
The first buffer in the list will be the last recently used
buffer that is not the current buffer unless
`helm-allow-skipping-current-buffer' is nil."
  (save-window-excursion
    (helm-frame-or-window-configuration 'restore)
    (let ((ido-process-ignore-lists t)
          ido-ignored-list)
      (ido-make-buffer-list nil))))

(defun helm-bookmarks-and-set ()
  "Preconfigured `helm' for bookmarks."
  (interactive)
  (helm-other-buffer
   '(helm-c-source-bookmarks helm-c-source-bookmark-set)
   "*helm bookmarks*"))


(define-key esc-map [remap find-tag] 'helm-semantic-or-imenu)
(global-set-key [remap find-tag] 'helm-semantic-or-imenu)

(define-key help-map [remap apropos-command] 'helm-c-apropos)
(global-set-key [remap apropos-command] 'helm-c-apropos)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(when (boundp 'ido-minor-mode-map-entry)
  (define-key (cdr ido-minor-mode-map-entry)
    [remap ido-switch-buffer]
    'helm-buffers-list))

;;; local-conf-helm.el ends here
