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

(defvar tmp-dir (file-name-as-directory (concat user-emacs-directory "tmp"))
  "Directory for temporary Emacs files.")
(make-directory tmp-dir t)

(setq custom-file (concat user-emacs-directory "my-custom-values.el"))

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

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
