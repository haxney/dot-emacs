;; For libraries which are autoloaded without any additional configuration.

;; ---- Quack
;; Provides Scheme mode
(autoload 'scheme-mode "quack" "Enter scheme-mode." t)
(add-to-list 'auto-mode-alist '("\\.ss$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))

;; ---- Git
(autoload 'git-status "git" "Enter git-status mode" t)

;; ---- Dot
(autoload 'graphviz-dot-mode "graphviz-dot-mode" "Enter graphviz-dot-mode." t)
(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))

;; ---- Gri-mode
(autoload 'gri-mode "gri-mode" "Enter Gri-mode." t)
(add-to-list 'auto-mode-alist '("\\.gri$" . gri-mode))

;; ---- redo
(autoload 'redo "redo" "Redo things!" t)
(global-set-key (kbd "M-/") 'redo)

;; ---- ruby
(autoload 'ruby-mode "ruby-mode" "Ruby edit mode" t)
(autoload 'ruby-electric-mode "ruby-electric" "Ruby electric mode" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

;; for the  Ruby interpreter:
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

;; ---- psvn
;; Emacs interface for subversion.
(autoload 'svn-status "psvn" "svn-status mode" t)

;; ---- PGG
;; Encryption functions.
(autoload 'pgg-invoke "pgg" "Use a PGG command")

;; ---- Planner
(autoload 'plan "planner" "Planner mode" t)

;; ---- mmm-mode
(autoload 'mmm-mode "mmm-mode" "Mulitple Minor Mode" t)

;; ---- CSS mode
(autoload 'css-mode "css-mode" "Enter CSS-mode." t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

;; ---- Flyspell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;; ---- Flymake
(autoload 'flymake-find-file-hook "flymake" "On-the-fly syntax checker." t)
(autoload 'flymake-mode "flymake" "On-the-fly syntax checker." t)

;; ---- YAML mode
(autoload 'yaml-mode "yaml-mode" "Major mode for editing YAML files." t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; ---- Ditz mode
(autoload 'ditz-todo "ditz" "Ditz bug tracking system." t)
