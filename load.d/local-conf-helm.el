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
      anything-candidate-number-limit 50)

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

(defun local-conf-helm-descbinds-set (sym val)
  "The function for the `defcustom' `:set' keyword."
  (if val
      (progn
        (define-key help-map [remap describe-bindings] 'helm-descbinds)
        (global-set-key [remap describe-bindings] 'helm-descbinds))
    (define-key help-map [remap describe-bindings] nil)
    (global-unset-key [remap describe-bindings]))
  (set-default sym val))

(define-key esc-map [remap find-tag] 'helm-semantic-or-imenu)
(global-set-key [remap find-tag] 'helm-semantic-or-imenu)

(define-key help-map [remap apropos-command] 'helm-c-apropos)
(global-set-key [remap apropos-command] 'helm-c-apropos)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(when ido-minor-mode-map-entry
  (define-key (cdr ido-minor-mode-map-entry)
    [remap ido-switch-buffer]
    'helm-buffers-list))

;;; local-conf-helm.el ends here
