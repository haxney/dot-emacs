;; Must be set before loading ido.
(setq ido-save-directory-list-file (concat conf-tmp "ido.last"))

(when (string-match "^21\\." emacs-version)
  (require 'ido))

(ido-mode t)
