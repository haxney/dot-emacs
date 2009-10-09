(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -shell-escape")
 '(LaTeX-mode-hook (quote (flymake-mode-off turn-on-flyspell)) t)
 '(TeX-master nil)
 '(ac-dwim t)
 '(ac-override-local-map t)
 '(after-save-hook (quote (executable-make-buffer-file-executable-if-script-p)))
 '(anything-c-adaptive-history-file "~/.emacs.d/tmp/anything-c-adaptive-history")
 '(anything-su-or-sudo "sudo")
 '(auto-install-directory "~/.emacs.d/elisp/auto-install/")
 '(auto-save-list-file-prefix "~/.emacs.d/tmp/auto-save-list/.saves-")
 '(backup-directory-alist (quote (("." . "~/.emacs.d/tmp/baks/"))))
 '(bcc-blacklist (quote ("/\\.recentf$" "/history$" "auctex")))
 '(bcc-cache-directory "~/.emacs.d/tmp/byte-cache")
 '(blink-cursor-mode nil)
 '(bm-annotate-on-create nil)
 '(bm-buffer-persistence t)
 '(bm-highlight-style (quote bm-highlight-only-fringe))
 '(bm-recenter t)
 '(bm-repository-file "~/.emacs.d/tmp/bm-repository")
 '(bm-restore-repository-on-load t)
 '(bookmark-default-file "~/.emacs.d/tmp/bookmark")
 '(browse-url-firefox-program "firefox-3.5")
 '(case-fold-search t)
 '(column-number-mode t)
 '(company-backends (quote (company-elisp company-nxml company-css company-eclim company-semantic company-xcode company-ropemacs (company-etags company-dabbrev-code company-keywords) company-oddmuse company-files company-dabbrev)))
 '(company-begin-commands (quote (self-insert-command)))
 '(company-idle-delay t)
 '(company-major-modes (quote (css-mode emacs-lisp-mode nxml-mode text-mode)))
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(cua-delete-selection nil)
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(current-language-environment "UTF-8")
 '(default-frame-alist (quote ((cursor-type bar . 2) (font . "Monospace-8"))))
 '(default-input-method "rfc1345")
 '(default-major-mode (quote text-mode))
 '(ecb-layout-name "left8")
 '(ecb-layout-window-sizes (quote (("left8" (0.22137404580152673 . 0.28125) (0.22137404580152673 . 0.25) (0.22137404580152673 . 0.265625) (0.22137404580152673 . 0.1875)))))
 '(ecb-options-version "2.32")
 '(ecb-tip-of-the-day nil)
 '(ede-project-placeholder-cache-file "~/.emacs.d/tmp/projects.ede")
 '(erc-log-channels-directory "~/logs")
 '(erc-log-insert-log-on-open nil)
 '(erc-log-mode t)
 '(erc-log-write-after-insert t)
 '(erc-log-write-after-send t)
 '(erc-modules (quote (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring smiley stamp spelling track)))
 '(erc-pals (quote ("evie")))
 '(eshell-before-prompt-hook (quote (smart-tab-mode-off)))
 '(eshell-directory-name "~/.emacs.d/tmp/eshell/")
 '(eshell-mode-hook (quote (visual-line-mode)))
 '(fill-column 80)
 '(flymake-allowed-file-name-masks (quote (("\\.c\\'" flymake-simple-make-init) ("\\.cpp\\'" flymake-simple-make-init) ("\\.xml\\'" flymake-xml-init) ("\\.cs\\'" flymake-simple-make-init) ("\\.p[ml]\\'" flymake-perl-init) ("\\.php[345]?\\'" flymake-php-init) ("\\.module\\'" flymake-php-init) ("\\.inc\\'" flymake-php-init) ("\\.install\\'" flymake-php-init) ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup) ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup) ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup) ("\\.tex\\'" flymake-simple-tex-init) ("\\.idl\\'" flymake-simple-make-init))))
 '(flyspell-use-meta-tab nil)
 '(geben-temporary-file-directory "~/.emacs.d/tmp/geben")
 '(git-append-signed-off-by t)
 '(global-auto-complete-mode t)
 '(global-company-mode t)
 '(global-linum-mode t)
 '(global-semantic-folding-mode t nil (semantic-util-modes))
 '(global-semantic-idle-completions-mode nil nil (semantic-idle))
 '(global-semantic-idle-tag-highlight-mode t nil (semantic-idle))
 '(global-semantic-show-parser-state-mode t nil (semantic-util-modes))
 '(global-semantic-stickyfunc-mode t nil (semantic-util-modes))
 '(global-semantic-tag-folding-mode t nil (semantic-util-modes))
 '(global-smart-tab-mode t)
 '(hippie-expand-dabbrev-as-symbol nil)
 '(hippie-expand-try-functions-list (quote (try-complete-file-name-partially try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially)))
 '(ido-everywhere t)
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "_whizzy_.*" ".*\\.raux" ".*\\.wdvi" "\\.git")))
 '(ido-minibuffer-setup-hook (quote ((lambda nil (local-set-key "" (quote ido-prev-match))) (lambda nil (local-set-key "" (quote ido-next-match))))))
 '(ido-rotate-file-list-default t)
 '(ido-save-directory-list-file "~/.emacs.d/tmp/.ido.last")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(install-elisp-repository-directory "~/.emacs.d/elisp")
 '(jde-ant-enable-find t)
 '(jde-ant-read-target t)
 '(jde-build-function (quote (jde-ant-build)))
 '(jde-complete-function (quote jde-complete-minibuf))
 '(jde-complete-unique-method-names nil)
 '(jde-enable-abbrev-mode t)
 '(jde-jdk (quote ("1.5")))
 '(jde-jdk-registry (quote (("1.5" . "/usr/lib/jvm/java-6-sun/"))))
 '(line-number-mode t)
 '(load-home-init-file t t)
 '(magit-commit-signoff t)
 '(menu-bar-mode nil)
 '(mode-require-final-newline (quote visit-save))
 '(nxhtml-skip-welcome t)
 '(nxml-mode-hook (quote (nxml-enable-unicode-char-name-sets rng-nxml-mode-init turn-on-flyspell)))
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-custom-commands (quote (("p" tags "PROJECT-MAYBE-DONE" nil))))
 '(org-agenda-files (quote ("~/org/root.org")))
 '(org-archive-location "%s_archive::* Archived Tasks")
 '(org-default-notes-file "~/org/root.org")
 '(org-id-locations-file "~/.emacs.d/tmp/.org-id-locations")
 '(org-registry-file "~/.emacs.d/tmp/org-registry.el")
 '(org-remember-templates (quote (("Todo" 116 "** TODO %?
  %i
 %a" "~/org/root.org" "") ("Appointment" 97 "** Appointment: %?
%^T
%i
  %a" "~/org/root.org" "") ("Blog Idea" 98 "*** %^{Title}
    %i%?" "~/org/root.org" "Blog Ideas") ("Music" 109 "*** %^{Song title} - %^{Artist}%!" "~/org/music.org" "To buy"))))
 '(org-return-follows-link t)
 '(org-todo-keywords (quote ((sequence "TODO" "STARTED" "WAITING" "DONE"))))
 '(org-track-directory "~/.emacs.d/elisp/")
 '(org-track-remove-package t)
 '(persp-completing-func (quote ido-completing-read))
 '(pgg-cache-passphrase nil)
 '(pgg-default-user-id "A016D1D6")
 '(planner-reverse-chronological-notes nil)
 '(pop-up-windows nil)
 '(quack-default-program "mzscheme -g")
 '(quack-global-menu-p nil)
 '(quack-pretty-lambda-p nil)
 '(quack-run-scheme-always-prompts-p nil)
 '(quack-smart-open-paren-p nil)
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-data-file "~/org/root.org")
 '(remember-handler-functions (quote (org-remember-handler)))
 '(remember-mode-hook (quote (turn-on-flyspell turn-on-auto-fill org-remember-apply-template)))
 '(require-final-newline (quote visit-save))
 '(save-completions-file-name "~/.emacs.d/tmp/completions")
 '(server-switch-hook (quote (turn-on-flyspell)))
 '(show-paren-mode t)
 '(smart-tab-using-hippie-expand t)
 '(smex-save-file "~/.emacs.d/tmp/smex.save")
 '(smiley-data-directory "~/.emacs.d/smileys")
 '(tab-width 4)
 '(text-mode-hook (quote (turn-on-flyspell turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(tramp-completion-reread-directory-timeout 5)
 '(tramp-default-method "ssh")
 '(tramp-persistency-file-name "~/.emacs.d/tmp/tramp")
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(udev-cedet-load-cedet (quote (excessive-code-helpers sem-ia)) nil (udev-cedet))
 '(udev-ecb-load-ecb t nil (udev-ecb))
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(windmove-wrap-around t)
 '(woman-use-own-frame nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(bm-face ((((class color) (background light)) (:background "orange1"))))
 '(bm-fringe-face ((((class color) (background light)) (:background "orange1"))))
 '(bm-fringe-persistent-face ((((class color) (background light)) (:background "DarkBlue"))))
 '(bm-persistent-face ((((class color) (background light)) (:background "DarkBlue"))))
 '(diff-added ((t (:inherit diff-changed-face :background "Green"))))
 '(diff-added-face ((t (:inherit diff-changed-face :background "Green"))) t)
 '(diff-removed ((t (:inherit diff-changed-face :background "red"))))
 '(diff-removed-face ((t (:inherit diff-changed-face :background "red"))) t)
 '(flyspell-duplicate ((((class color)) (:foreground "Gold3" :underline t :weight bold))))
 '(flyspell-incorrect ((((class color)) (:foreground "magenta" :underline t :weight bold))))
 '(whizzy-point-face ((((class color)) nil))))
