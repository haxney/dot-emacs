;;; init.el --- Where all the magic begins
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;;; Commentary:

;; Load path etc.

;;; Code:

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

;; Force loading this immediately. It overwrites package-specific paths to keep
;; ~/.emacs.d tidy.
(use-package no-littering
  :ensure t
  :demand t
  :config (no-littering-theme-backups))

(setq custom-file (concat user-emacs-directory "my-custom-values.el"))
(load custom-file)

(use-package load-dir)

(defgroup local-conf nil
  "A group for all of my local configuration.

It's not unreasonable to think that this may get split out into
its own package someday."
  :group 'local
  :prefix "local-conf-"
  :tag "Local configuration")

;;; init.el ends here
