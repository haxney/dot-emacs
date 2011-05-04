(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "C-!")
       '(lambda () (interactive) (dired-do-shell-command "totem" nil (dired-get-marked-files t current-prefix-arg))))))
