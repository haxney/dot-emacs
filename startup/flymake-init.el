;; Load flymake if it is applicable to the current file
(add-hook 'find-file-hook 'flymake-find-file-hook)

(eval-after-load "flymake"
  '(progn

     ;; LaTeX
     (defun flymake-get-tex-args (file-name)
       (list "chktex" (list "-q" "-v0" file-name)))))
