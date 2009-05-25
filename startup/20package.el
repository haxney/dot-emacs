;;; 20package.el --- Load `package' library.
;;
;; Author: Daniel Hackney
;; Copyright (C) 2009 Daniel Hackney

;;; Commentary:
;;
;;  Load package before loading startup files, since some of them may depend on
;;  package being loaded

(require 'package)
(package-initialize)

(require 'auto-install)
(add-to-list 'load-path auto-install-directory)

;;; 20package.el ends here
