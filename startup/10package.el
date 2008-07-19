;;; 01package.el --- Load `package' library.
;;
;; Filename: 01package.el
;; Description: Load `package' library.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Copyright (C) 2008, Daniel Hackney, all rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Load package before loading startup files, since some of them may depend on
;;  package being loaded

(require 'package)
(package-initialize)
