(require 'planner)
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
 '(LaTeX-command "latex -shell-escape")
 '(case-fold-search t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(git-append-signed-off-by t)
 '(jde-ant-enable-find t)
 '(jde-ant-read-target t)
 '(jde-build-function (quote (jde-ant-build)))
 '(jde-complete-function (quote jde-complete-minibuf))
 '(jde-complete-unique-method-names nil)
 '(jde-enable-abbrev-mode t)
 '(jde-jdk (quote ("1.5")))
 '(jde-jdk-registry (quote (("1.5" . "/usr/lib/jvm/java-6-sun/"))))
 '(load-home-init-file t t)
 '(nxml-slash-auto-complete-flag t)
 '(pgg-cache-passphrase nil)
 '(pgg-default-user-id "A016D1D6")
 '(planner-reverse-chronological-notes nil)
 '(quack-default-program "mzscheme -g")
 '(quack-global-menu-p nil)
 '(quack-pretty-lambda-p nil)
 '(quack-run-scheme-always-prompts-p nil)
 '(quack-smart-open-paren-p nil)
 '(show-paren-mode t nil (paren))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(transient-mark-mode t)
 '(truncate-lines t))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(diff-added-face ((t (:inherit diff-changed-face :background "Green"))))
 '(diff-removed-face ((t (:inherit diff-changed-face :background "red"))))
 '(flyspell-duplicate-face ((((class color)) (:foreground "Gold3" :underline t :weight bold))))
 '(flyspell-incorrect-face ((((class color)) (:foreground "magenta" :underline t :weight bold)))))


; tramp -- for remote access of files, ssh preferred access method
(require 'tramp)
(setq tramp-default-method "ssh")

; psvn -- Emacs interface for subversion
(autoload 'svn-status "psvn" "svn-status mode" t)

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
(autoload 'ruby-electric-mode "ruby-electric" "Ruby electric mode" t)
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
(autoload 'css-mode "css-mode" "Enter CSS-mode." t)
(setq auto-mode-alist (cons '("\\.css$" . css-mode) auto-mode-alist))

;; Planner
(setq planner-project "Journal of Hax")

(setq muse-project-alist
      '(("Journal of Hax"
	 ("~/Journal"           ;; where your Planner pages are located
	  :default "Index"      ;; use value of `planner-default-page'
	  :major-mode planner-mode
	  :visit-link planner-visit-link)

	 ;; This next part is for specifying where Planner pages
	 ;; should be published and what Muse publishing style to
	 ;; use.  In this example, we will use the XHTML publishing
	 ;; style.

	 (:base "planner-xhtml"
		;; where files are published to
		;; (the value of `planner-publishing-directory', if
		;;  you have a configuration for an older version
		;;  of Planner)
		:path "~/Journal/html"))))

(require 'planner-publish)

;; Include remember
(require 'remember-planner)
(setq remember-handler-functions '(remember-planner-append))
(setq remember-annotation-functions planner-annotation-functions)

;; Bind remember to C-c C-r
(global-unset-key (kbd "C-c C-n"))
(global-set-key (kbd "C-c C-n") 'remember)

(add-hook 'planner-mode-hook 'flyspell-mode)

; Set up planner template
(defun dhackney/planner-daily-template ()
  "Build a new daily planner page."
  (insert (concat "#title Journal Entry for "
	  (format-time-string "%A, %B %e, %Y")
	  "\n\n* Tasks\n\n\nWake: Up: \n\n* Events\n")))

(setq planner-day-page-template 'dhackney/planner-daily-template)

; Run planner on startup
(plan)

; Cscope maintains information about C programs.
(require 'xcscope)

; Fix jde overlay
(require 'overlay-fix)

;; ----- nXML

; Add new schemas to nXML
(push "~/.emacs.d/schemas/schemas.xml" rng-schema-locating-files-default)

; Spelling in nXML
(add-to-list 'flyspell-prog-text-faces 'nxml-text-face)

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

;; Quack - for Scheme mode
(autoload 'scheme-mode "quack" "Enter scheme-mode." t)
(setq auto-mode-alist (append '(("\\.ss$" . scheme-mode)
                                ("\\.scm$" . scheme-mode))
                              auto-mode-alist))

;; ---- Tab settings
(setq-default tab-width 4 indent-tabs-mode nil)

;; ---- Git mode
(autoload 'git-status "git" "Enter git-status mode" t)

;; ---- Dot mode
(autoload 'graphviz-dot-mode "graphviz-dot-mode" "Enter graphviz-dot-mode." t)
(setq auto-mode-alist (cons '("\\.dot$" . graphviz-dot-mode) auto-mode-alist))

;; ---- PGG (encryption) keys
(autoload 'pgg-invoke "pgg" "Use a PGG command")

(defun dhackney/pgg-encrypt-sign (rcpts &optional sign start end passphrase)
  "Sign and encrypt the buffer."
  (interactive (list (split-string (read-string "Recipients: ") "[ \t,]+") t))
  (let* ((start (or start (point-min)))
	 (end (or end (point-max)))
	 (status (pgg-encrypt-region start end rcpts sign passphrase)))
    (when (interactive-p)
      (pgg-display-output-buffer start end status))
    status))

(global-set-key (kbd "C-c / e") 'dhackney/pgg-encrypt-sign)

;; ---- Gri-mode
; Load on demand rather than at initialization.
(autoload 'gri-mode "gri-mode" "Enter Gri-mode." t)
(setq auto-mode-alist (cons '("\\.gri$" . gri-mode) auto-mode-alist))