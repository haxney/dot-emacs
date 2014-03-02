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

(setq debug-on-error t
      ;; debug-on-signal t
      debug-on-quit t)
;; Load cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'req-package)

(defvar tmp-dir (file-name-as-directory (concat user-emacs-directory "tmp"))
  "Directory for temporary Emacs files.")

(make-directory tmp-dir t)

(setq custom-file (concat user-emacs-directory "custom.el"))

;; Force `load-dir' package to load directories without having to wait for
;; custom to finish loading.
(use-package load-dir
  :ensure load-dir
  :init (progn
          (setq load-dirs t)
          (load-dirs)))

(req-package-finish)

(load custom-file)

(when (file-exists-p "~/Private/private.el.gz.gpg")
  (load "~/Private/private.el.gz.gpg"))

(defgroup local-conf nil
  "A group for all of my local configuration.

It's not unreasonable to think that this may get split out into
its own package someday."
  :prefix "local-conf-"
  :tag "Local configuration")

;;; init.el ends here
