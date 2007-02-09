
(add-to-list 'load-path "~/.emacs.d/elisp")

; Set good font!
(set-frame-font "-Adobe-Courier-Medium-R-Normal--17-120-100-100-M-100-ISO8859-1")

; Line numbers
(line-number-mode 1)
(column-number-mode 1)

; Fill column width
(setq-default fill-column 80)

; tramp -- for remote access of files, ssh preferred access method
; since the cslab computers don't have it, let's not use tramp for now
; (require 'tramp)
; (setq tramp-default-method "ssh")

; psvn -- Emacs interface for subversion
(require 'psvn)

; Don't wrap lines, truncate them instead, but not for term mode
(setq-default truncate-lines t)
(add-hook 'term-mode-hook
	  '(lambda () (setq truncate-lines nil)))

; syntax highlighting by default (needs to be done before ruby-electric)
(load "font-lock")
(global-font-lock-mode)

; .rhtml loads html
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))

; load mmm-mode rails support
;(load "~/.emacs.d/mmm-mode_init")

; Ruby help
(require 'ruby-electric)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(defun my-ruby-mode-hook ()
  (ruby-electric-mode)
  (font-lock-mode)
  (pabbrev-mode))
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

;; Load Pabbrev
(require 'pabbrev)
(global-pabbrev-mode)
; do we need to disable it in term mode?

; don't clutter directories!
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/baks"))))
(setq auto-save-directory (expand-file-name "~/.emacs.d/baks"))

;; for the  Ruby interpreter:
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
      '(lambda ()
         (inf-ruby-keys)
))

; Ri-Emacs support
(setq ri-ruby-script "~/.elisp/ri-emacs.rb")
(autoload 'ri "~/.elisp/ri-ruby.el" nil t)

(add-hook 'ruby-mode-hook (lambda ()
;                               (local-set-key 'f3 'ri)
                               (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
;                               (local-set-key 'f4 'ri-ruby-show-args)
                               ))



; CSS mode
(require 'css-mode)
(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(global-font-lock-mode t nil (font-lock))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )
