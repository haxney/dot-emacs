(when (string-match "^21\\." emacs-version)
  (require 'ido))

(setq ido-save-directory-list-file (concat conf-tmp "ido.last"))

(ido-mode t)
