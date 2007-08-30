(add-to-list 'load-path "~/.emacs.d/elisp")

; Set good font!
;(set-frame-font "-*-Courier-Medium-R-Normal--14-*-*-*-M-*-*")

; Line numbers
(line-number-mode 1)
(column-number-mode 1)

; Fill column width
(setq-default fill-column 80)

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(case-fold-search t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(jde-ant-enable-find t)
 '(jde-ant-read-target t)
 '(jde-build-function (quote (jde-ant-build)))
 '(jde-complete-function (quote jde-complete-minibuf))
 '(jde-complete-unique-method-names nil)
 '(jde-jdk (quote ("1.5")))
 '(jde-jdk-registry (quote (("1.5" . "/usr/lib/jvm/java-6-sun/"))))
 '(load-home-init-file t t)
 '(nxml-slash-auto-complete-flag t)
 '(planner-reverse-chronological-notes nil)
 '(show-paren-mode t nil (paren))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(transient-mark-mode t)
 '(truncate-lines t))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(flyspell-duplicate-face ((((class color)) (:foreground "Gold3" :underline t :weight bold))))
 '(flyspell-incorrect-face ((((class color)) (:foreground "magenta" :underline t :weight bold)))))

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

; Turn on auto-fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)

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
;(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/baks"))))
(setq auto-save-directory (expand-file-name "~/.emacs.d/autosave"))

; create a backup file directory
(defun make-backup-file-name (file)
(concat "~/.emacs.d/baks/" (file-name-nondirectory file) "~"))

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

; Cscope maintains information about C programs.
(require 'xcscope)

; Fix jde overlay
(require 'overlay-fix)

;; ---- Remove bad Gui settings.

;; Remove menu and toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

; Set F5 to replay last macro
(global-set-key [f5] 'call-last-kbd-macro)

;; ---- key rebindings

; Rebind M-x to C-x C-m
(global-set-key "\C-x\C-m" 'execute-extended-command)

; Set C-w to backward kill word and remap existing C-w to C-x C-k
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

(global-set-key "\C-c\C-v\C-c" 'compile)

;; Place semantic.cache files somewhere central
(setq semanticdb-default-save-directory "~/.emacs.d/semantic-cache")

;; Speedbar settings
(global-set-key "\C-co" 'speedbar-get-focus)
