;;; 60flymake-ruby.el --- Ruby settings for flymake.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Ruby settings for flymake.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: ruby flymake local

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  Set colors and filenames for Ruby Flymake.

;;; Code:

(eval-after-load 'flymake
  '(progn
     ;; I don't like the default colors :)
     (set-face-background 'flymake-errline "red4")
     (set-face-background 'flymake-warnline "dark slate blue")

     ;; Invoke ruby with '-c' to get syntax checking
     (defun flymake-ruby-init ()
       (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                            'flymake-create-temp-inplace))
              (local-file  (file-relative-name
                            temp-file
                            (file-name-directory buffer-file-name))))
         (list "ruby" (list "-c" local-file))))

     (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

     (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

     (add-hook 'ruby-mode-hook
               '(lambda ()

                  ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
                  (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                      (flymake-mode))
                  ))))

;;; 60flymake-ruby.el ends here
