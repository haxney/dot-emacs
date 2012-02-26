;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
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

(defun my/message-startup-time ()
  "Display a message of how long Emacs took to start up, in milliseconds."
  (message "Emacs loaded in %dms"
           (/ (-
               (+
                (third after-init-time)
                (* 1000000
                   (second after-init-time)))
               (+
                (third before-init-time)
                (* 1000000
                   (second before-init-time))))
              1000)))

(add-hook 'after-init-hook 'my/message-startup-time)

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

(defalias 'yes-or-no-p 'y-or-n-p)


(setq custom-file (concat dotfiles-dir "custom.el"))
(defun load-custom-file ()
  (load custom-file))

(defun do-uncooperative-requires ()
  "Manually load packages without `autoloads'."

  (require 'undo-tree)
  (require 'ess-site nil t)
  (require 'keyfreq))

(add-hook 'after-init-hook 'do-uncooperative-requires)
(add-hook 'after-init-hook 'smex-initialize)

;; Doing this seems to be important. Some stuff is not set up for customize to
;; act until after packages and such are loaded, but customize needs to set up
;; in order for those things to work. It's all very strange.
(add-hook 'after-init-hook 'load-custom-file 'append)

(when (file-exists-p "~/Private/private.el")
  (load "~/Private/private"))

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
                          (descbinds-anything t)
                          (anything-match-plugin "1.3.9")
                          (anything-config "1.3.9")
                          (anything "1.3.9")
                          (full-ack t)
                          (coffee-mode t)
                          (mode-compile t)
                          (less t)
                          (ess t)
                          ))

;; Autoloads for ESS are whack, need to load this manually
;;(require 'ess-site)

(defvar hl-line-ignore-regexp "\*magit:.*")

(defadvice global-hl-line-highlight (around unhighlight-some-buffers
                                      ()
                                      activate)
       "Don't highlight in buffers which match a regexp."
       (unless (string-match hl-line-ignore-regexp (buffer-name (window-buffer (selected-window))))
         ad-do-it))

(defadvice less-minor-mode (after disable-less-minor-mode-read-only
                                      ()
                                      activate)
       "`less-minor-mode' makes the buffer read-only. This is silly"
       (when less-minor-mode
         (setq buffer-read-only nil)))

;;; init.el ends here
