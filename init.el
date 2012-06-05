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
      tmp-dir (file-name-directory (concat dotfiles-dir "tmp/")))

(make-directory tmp-dir t)
(setq load-dirs t) ; Force `load-dir' package to load directories without having
                   ; to wait for custom to finish loading.

(when (file-exists-p (concat dotfiles-dir "nxhtml/autostart.el"))
  (load (concat dotfiles-dir "nxhtml/autostart")))

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

(add-hook 'after-init-hook 'do-uncooperative-requires)
(add-hook 'after-init-hook 'smex-initialize)

;; Doing this seems to be important. Some stuff is not set up for customize to
;; act until after packages and such are loaded, but customize needs to set up
;; in order for those things to work. It's all very strange.
(add-hook 'after-init-hook 'load-custom-file 'append)
(add-hook 'after-init-hook 'really-activate-desktop 'append)

(when (file-exists-p "~/Private/private.el.gz.gpg")
  (load "~/Private/private.el.gz.gpg"))

;; Explicitly list packages to be loaded and used.

(setq package-load-list '((yaml-mode t)
                          (undo-tree t)
                          (smex t)
                          (scss-mode t)
                          (scala-mode t)
                          (sass-mode t)
                          (ruby-test-mode t)
                          (ruby-mode t)
                          (ruby-electric t)
                          (ruby-compilation t)
                          (ruby-block t)
                          (rspec-mode t)
                          (rinari t)
                          (paredit t)
                          (org-table-comment t)
                          (org-magit t)
                          (org t)
                          (mediawiki t)
                          (markdown-mode t)
                          (magit t)
                          (load-dir t)
                          (keyfreq t)
                          (inflections t)
                          (jump t)
                          (inf-ruby t)
                          (htmlize t)
                          (haml-mode t)
                          (gnuplot t)
                          (geben t)
                          (furl t)
                          (findr t)
                          (feature-mode t)
                          (erc-nick-notify t)
                          (diminish t)
                          (auctex t)
                          (full-ack t)
                          (coffee-mode t)
                          (mode-compile t)
                          (less t)
                          (ess t)
                          (highline t)
                          (helm t)
                          (helm-descbinds t)
                          (tidy t)
                          (company t)
                          ))

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

;;; init.el ends here
