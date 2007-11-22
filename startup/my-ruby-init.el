;; Initialization for Ruby modes and options.

;; Ruby help
(autoload 'ruby-mode "ruby-mode" "Ruby edit mode" t)
(autoload 'ruby-electric-mode "ruby-electric" "Ruby electric mode" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(add-hook 'ruby-mode-hook 'ruby-electric-mode)

;; .rhtml loads html
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))

;; for the  Ruby interpreter:
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")

(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)))

;; Ri-Emacs support
;;(setq ri-ruby-script "~/.elisp/ri-emacs.rb")
;;(autoload 'ri "~/.elisp/ri-ruby.el" nil t)
