;;; 10custom.el --- Load `custom-file'.
;;
;; Filename: 10custom.el
;; Description: Load `custom-file.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Copyright (C) 2008, Daniel Hackney, all rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Store custom settings in a different file.

(setq custom-file (concat conf-home "my-custom.el"))
(load custom-file 'noerror)
