(defvar *emacs-load-start* (current-time))

(setq conf-home (concat (file-name-as-directory (expand-file-name "~"))
						(file-name-as-directory ".emacs.d")))

(add-to-list 'load-path (concat conf-home "elisp"))

;; Load all files in ~/.emacs.d/startup"
(let ((startup-files (directory-files
					  (concat conf-home
							  (file-name-as-directory "startup")) t "^[^.]")))
    (mapc (lambda (file)
		  (load file))
		  startup-files))

;; psvn -- Emacs interface for subversion
(autoload 'svn-status "psvn" "svn-status mode" t)

;; Don't wrap lines, truncate them instead, but not for term mode
(setq-default truncate-lines t)
(add-hook 'term-mode-hook
          '(lambda () (setq truncate-lines nil)))

;; load mmm-mode rails support
;;(load "~/.emacs.d/mmm-mode_init")

;; Load Pabbrev
(require 'pabbrev)
(global-pabbrev-mode)
;; do we need to disable it in term mode?

;; don't clutter directories!
(setq auto-save-directory (concat conf-home "autosave"))

;; create a backup file directory
(defun make-backup-file-name (file)
  (concat conf-home
		  (file-name-as-directory "baks")
		  (file-name-nondirectory file)
		  "~"))

;; CSS mode
(autoload 'css-mode "css-mode" "Enter CSS-mode." t)
(setq auto-mode-alist (cons '("\\.css$" . css-mode) auto-mode-alist))

;; Cscope maintains information about C programs.
(autoload 'c-mode "xcscope" "Cscope for C" t)

;; Fix jde overlay
(require 'overlay-fix)

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;; ----- nXML

(eval-after-load "nxml"
  '(progn
     ;; Spelling in nXML
     (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)

     ;; Add new schemas to nXML
     (push (concat conf-home
				   (file-name-as-directory "schemas")
				   "schemas.xml")
		   rng-schema-locating-files-default)))

;; Set F5 to replay last macro
(global-set-key [f5] 'call-last-kbd-macro)

;; ---- key rebindings

;; Rebind M-x to C-x C-m
(global-set-key "\C-x\C-m" 'execute-extended-command)

;; Set C-w to backward kill word and remap existing C-w to C-x C-k
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

;; Globally set C-c C-v C-c to compile.
(global-set-key "\C-c\C-v\C-c" 'compile)

;; Place semantic.cache files somewhere central
(setq semanticdb-default-save-directory
	  (concat conf-home "semantic-cache"))

;; Speedbar settings
(global-set-key "\C-co" 'speedbar-get-focus)

;; Quack - for Scheme mode
(autoload 'scheme-mode "quack" "Enter scheme-mode." t)
(setq auto-mode-alist (append '(("\\.ss$" . scheme-mode)
                                ("\\.scm$" . scheme-mode))
                              auto-mode-alist))

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
(autoload 'gri-mode "gri-mode" "Enter Gri-mode." t)
(setq auto-mode-alist (cons '("\\.gri$" . gri-mode) auto-mode-alist))

;; ---- redo
(autoload 'redo "redo" "Redo things!" t)
(global-set-key (kbd "M-/") 'redo)

;; ---- Emacsclient
(server-start)

;; ---- AUCTeX
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Store custom settings in a different file.
(setq custom-file
	  (concat conf-home "my-custom.el"))
(load custom-file 'noerror)

;; Cool buffer switcher.
(when (string-match "^22\\." emacs-version)
  (ido-mode t))

;; Time how long it took to start up.
(let ((the-time (current-time)))
  (message "My .emacs loaded in %dms"
           (/ (-
               (+
                (third the-time)
                (* 1000000
                   (second the-time)))
               (+
                (third *emacs-load-start*)
                (* 1000000
                   (second *emacs-load-start*)))
               ) 1000)))
