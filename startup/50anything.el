;;; 50anything.el --- Set up anything.el.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Set up anything.el.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: convenience abbrev local matching

;; This file is NOT part of GNU Emacs.

;;; Commentary:

(require 'anything)
(require 'anything-config)
(require 'anything-etags)
(require 'anything-complete)

;; Use anything for C-h b
(require 'descbinds-anything)
(descbinds-anything-install)

;;; Code:

;;; 50anything.el ends here
