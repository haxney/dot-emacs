;; Initialization for Ruby modes and options.
(add-hook 'ruby-mode-hook 'ruby-electric-mode)

;; .rhtml loads html
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)))
