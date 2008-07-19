;;; 50term-mode.el --- Settings for `term-mode'.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Settings for `term-mode'.el
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: convenience local terminals

;; This file is NOT part of GNU Emacs.

;;; Code:

(add-hook 'term-mode-hook
          '(lambda () (setq truncate-lines nil)))

;;; 50term-mode.el ends here
