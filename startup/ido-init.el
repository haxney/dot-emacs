(when (string-match "^21\\." emacs-version)
  (require 'ido))
(ido-mode t)