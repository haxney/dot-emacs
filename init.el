;;; init.el --- Where all the magic begins
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name))
      tmp-dir (file-name-directory (concat dotfiles-dir "tmp/"))
      cedet-dir (file-name-directory (concat dotfiles-dir "cedet-1.1/")))

(if (and (string= emacs-version "24.2.1")
         (file-exists-p (concat cedet-dir "common/cedet.el")))
    (progn
      (load (concat cedet-dir "common/cedet"))
      (require 'semantic-el nil t))
  (require 'semantic/bovine/el nil t))

(make-directory tmp-dir t)
(setq load-dirs t)
;; Force `load-dir' package to load directories without having to wait for
;; custom to finish loading.

(defun pretty-print-xml (begin end)
  "Pretty format XML markup in region.

You need to have `nxml-mode'
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this. The function inserts linebreaks to separate tags that have
nothing but whitespace between them. It then indents the markup
by using nXML's indentation rules."
  (interactive (list (if mark-active (region-beginning) (point-min))
                     (if mark-active (region-end) (point-max))))
  (save-excursion
    (save-match-data
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end)))
  (message "Ah, much better!"))

;; Re-enable narrow-to-region
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq custom-file (concat dotfiles-dir "custom.el"))
(defun load-custom-file ()
  (load custom-file))

(defun really-activate-desktop ()
  "Activate `desktop-read' after init.
desktop.el insists on putting its `after-init-hook' lambda ahead
of my custom loading, so `desktop-save-mode' is not set when it
runs. By forcing the read to happen after loading the custom
file, we can make sure that `desktop-read' is actually called
when needed."
  (let ((key "--no-desktop"))
    (when (member key command-line-args)
      (setq command-line-args (delete key command-line-args))
      (setq desktop-save-mode nil)))
  (when desktop-save-mode
    (desktop-read)
    (setq inhibit-startup-screen t)))

(run-at-time t (* 60 10) 'desktop-save-in-desktop-dir)

(defun do-uncooperative-requires ()
  "Manually load packages without `autoloads'."
  (require 'ess-site nil t)
  (require 'keyfreq))

(defun load-nxhtml ()
  "Load nxhtml"
  (when (file-exists-p (concat dotfiles-dir "nxhtml/autostart.el"))
    (load (concat dotfiles-dir "nxhtml/autostart"))))

(add-hook 'after-init-hook 'do-uncooperative-requires)
(add-hook 'after-init-hook 'smex-initialize)
;;(add-hook 'after-init-hook 'load-nxhtml)

(autoload 'dired-details "dired-details")
(autoload 'dired-details-install "dired-details")
(add-hook 'after-init-hook 'dired-details-install)

;; Doing this seems to be important. Some stuff is not set up for customize to
;; act until after packages and such are loaded, but customize needs to set up
;; in order for those things to work. It's all very strange.
(add-hook 'after-init-hook 'load-custom-file 'append)
(add-hook 'after-init-hook 'really-activate-desktop 'append)

(when (file-exists-p "~/Private/private.el.gz.gpg")
  (load "~/Private/private.el.gz.gpg"))

;; Explicitly list packages to be loaded and used.

(setq package-load-list
      '(
        (apache-mode t)      ;; marmalade
        (auctex t)           ;; gnu
        (auto-complete t)    ;; melpa
        (coffee-mode t)      ;; melpa
        (company t)          ;; gnu
        (csv-mode t)         ;; melpa
        (diminish t)         ;; marmalade
        (dired-details t)    ;; melpa
        (ess t)              ;; melpa
        (feature-mode t)     ;; marmalade
        (findr t)            ;; melpa
        (full-ack t)         ;; melpa
        (furl t)             ;; marmalade
        (geben t)            ;; marmalade
        (geiser t)           ;; elpa-haxney
        (gh t)               ;; melpa
        (gist t)             ;; melpa
        (gnuplot t)          ;; melpa
        (groovy-mode t)      ;; melpa
        (haml-mode t)        ;; melpa
        (helm t)             ;; melpa
        (helm-R t)           ;; melpa
        (helm-descbinds t)   ;; melpa
        (helm-git t)         ;; melpa
        (hl-line+ t)         ;; marmalade
        (hl-sexp t)          ;; melpa
        (htmlize t)          ;; marmalade
        (iedit t)            ;; melpa
        (inf-ruby t)         ;; melpa
        (inflections t)      ;; melpa
        (info+ t)            ;; melpa
        (jade-mode t)        ;; melpa
        (jump t)             ;; melpa
        (keyfreq t)          ;; marmalade
        (keywiz t)           ;; marmalade
        (less t)             ;; elpa
        (load-dir t)         ;; gnu
        (logito t)           ;; melpa
        (lua-mode t)         ;; melpa
        (magit t)            ;; melpa
        (markdown-mode t)    ;; melpa
        (mediawiki t)        ;; melpa
        (memory-usage t)     ;; gnu
        (mode-compile t)     ;; marmalade
        (multiple-cursors t) ;; melpa
        (nlinum t)           ;; gnu
        (paredit t)          ;; melpa
        (pcache t)           ;; melpa
        (php-mode t)         ;; melpa
        (popup t)            ;; melpa
        (psl-mode t)         ;; elpa-haxney
        (quack t)            ;; elpa-haxney
        (restclient t)       ;; melpa
        (rinari t)           ;; melpa
        (rspec-mode t)       ;; melpa
        (ruby-block t)       ;; melpa
        (ruby-compilation t) ;; melpa
        (ruby-mode t)        ;; melpa
        (ruby-test-mode t)   ;; marmalade
        (rvm t)              ;; melpa
        (sass-mode t)        ;; melpa
        (scala-mode t)       ;; melpa
        (scss-mode t)        ;; melpa
        (smart-mode-line t)  ;; melpa
        (smex t)             ;; melpa
        (smooth-scrolling t) ;; melpa
        (ssh-config-mode t)  ;; melpa
        (sws-mode t)         ;; melpa
        (tidy t)             ;; melpa
        (unbound t)          ;; marmalade
        (undo-tree t)        ;; gnu
        (vbnet-mode t)       ;; elpa-haxney
        (vimgolf t)          ;; melpa
        (vlf t)              ;; gnu
        (yaml-mode t)        ;; melpa
        ))

;; Needed by MELPA to avoid timeouts
(setq url-http-attempt-keepalives nil)

(defvar hl-line-ignore-regexp "\*magit:.*")

(defadvice global-hl-line-highlight (around unhighlight-some-buffers
                                      ()
                                      activate)
       "Don't highlight in buffers which match a regexp."
       (unless (string-match hl-line-ignore-regexp (buffer-name (window-buffer (selected-window))))
         ad-do-it))

(defadvice less-minor-mode (around less-minor-mode-respect-read-only
                                   ()
                                   activate)
  "`less-minor-mode' overrides the read-only status of the buffer.

That's not nice."
  (let ((old-read-only buffer-read-only))
    ad-do-it
    (setq buffer-read-only old-read-only)))

(eval-after-load 'info
  '(progn
     (define-key Info-mode-map (kbd ";") 'Info-next-reference)
     (define-key Info-mode-map (kbd "'") 'Info-prev-reference)))

;; Make data-debug much more useful: set `buffer-read-only' so that the nice
;; structure of the buffer doesn't get clobbered and `inhibit-read-only' so that
;; the collapse and expand commands can do their thing
(add-hook 'data-debug-mode-hook '(lambda () (setq buffer-read-only t)))

(mapc '(lambda (fun) (ad-add-advice fun
                                    '(data-debug-inhibit-read-only
                                      nil
                                      t
                                      (advice . (lambda () (let ((inhibit-read-only t))
                                                             ad-do-it))))
                                    'around
                                    0)
         (ad-activate fun))
      '(data-debug-next
        data-debug-prev
        data-debug-next-expando
        data-debug-prev-expando
        data-debug-expand-or-contract
        data-debug-expand-or-contract-mouse))

;; hack for now
(add-to-list 'load-path "/usr/share/emacs/site-lisp/ess")

(defgroup local-conf nil
  "A group for all of my local configuration.

It's not unreasonable to think that this may get split out into
its own package someday."
  :prefix "local-conf-"
  :tag "Local configuration")

;; I wish it was a defcustom
(setq mc/list-file "~/.emacs.d/tmp/.mc-lists.el")

;;; init.el ends here
