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

(load-file (concat dotfiles-dir "/nxhtml/autostart.el"))

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

(defun require-undo-tree ()
  "Load `undo-tree'."
  (require 'undo-tree))
(add-hook 'after-init-hook 'smex-initialize)
(add-hook 'after-init-hook 'require-undo-tree)  ; Doesn't provide autoloads :(

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(load "~/Private/private")

(setq custom-file (concat dotfiles-dir "custom.el"))
(defun load-custom-file ()
  (load custom-file 'noerror))

;; Doing this seems to be important. Some stuff is not set up for customize to
;; act until after packages and such are loaded, but customize needs to set up
;; in order for those things to work. It's all very strange.
(add-hook 'after-init-hook 'load-custom-file 'append)

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
                          (anything-match-plugin "1.3.4")
                          (anything-config "1.3.4")
                          (anything "1.3.4")
                          (keyfreq t)
                          (full-ack t)
                          (coffee-mode t)
                          (mode-compile t)
                          ))

;;; init.el ends here
