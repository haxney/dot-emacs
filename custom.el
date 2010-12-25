(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -shell-escape")
 '(LaTeX-mode-hook (quote (flymake-mode-off turn-on-flyspell)) t)
 '(TeX-master nil)
 '(after-save-hook (quote (executable-make-buffer-file-executable-if-script-p)))
 '(anything-c-adaptive-history-file "~/.emacs.d/tmp/anything-c-adaptive-history")
 '(anything-su-or-sudo "sudo")
 '(auto-save-list-file-prefix "~/.emacs.d/tmp/auto-save-list/.saves-")
 '(backup-directory-alist (quote (("." . "~/.emacs.d/tmp/baks/"))))
 '(before-save-hook (quote (delete-trailing-whitespace)))
 '(blink-cursor-mode nil)
 '(bm-annotate-on-create nil)
 '(bm-buffer-persistence t)
 '(bm-highlight-style (quote bm-highlight-only-fringe))
 '(bm-recenter t)
 '(bm-repository-file "~/.emacs.d/tmp/bm-repository")
 '(bm-restore-repository-on-load t t)
 '(bookmark-default-file "~/.emacs.d/tmp/bookmark")
 '(browse-url-firefox-program "firefox-3.5")
 '(case-fold-search t)
 '(column-number-mode t)
 '(company-backends (quote (company-elisp company-nxml company-css company-eclim company-semantic company-xcode company-predictive (company-etags company-keywords) company-oddmuse company-files)))
 '(company-begin-commands (quote (self-insert-command org-self-insert-command)))
 '(company-idle-delay t)
 '(company-major-modes (quote (css-mode emacs-lisp-mode nxml-mode lisp-interaction-mode org-mode log-edit-mode)))
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(cua-delete-selection nil)
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(current-language-environment "UTF-8")
 '(debug-on-error nil)
 '(default-frame-alist (quote ((cursor-type bar . 2) (font . "Monospace-8"))))
 '(default-input-method "rfc1345")
 '(default-major-mode (quote text-mode) t)
 '(ecb-layout-name "left8")
 '(ecb-layout-window-sizes (quote (("left8" (0.22137404580152673 . 0.28125) (0.22137404580152673 . 0.25) (0.22137404580152673 . 0.265625) (0.22137404580152673 . 0.1875)))))
 '(ecb-options-version "2.32")
 '(ecb-tip-of-the-day nil)
 '(ede-project-placeholder-cache-file "~/.emacs.d/tmp/projects.ede")
 '(edit-server-new-frame nil)
 '(emacs-lisp-mode-hook (quote (turn-on-eldoc-mode (lambda nil (setq mode-name "El")) (lambda nil (paredit-mode 1) (idle-highlight 1) (run-coding-hook)) esk-remove-elc-on-save)))
 '(erc-autoaway-mode t)
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#drupal-vcs" "#drupal" "#emacs" "#uzbl"))))
 '(erc-autojoin-mode t)
 '(erc-email-userid "dan@haxney.org")
 '(erc-generate-log-file-name-function (quote erc-generate-log-file-name-date-and-name))
 '(erc-log-channels-directory "~/logs")
 '(erc-log-insert-log-on-open nil)
 '(erc-log-mode t)
 '(erc-log-write-after-insert t)
 '(erc-log-write-after-send t)
 '(erc-max-buffer-size 20000)
 '(erc-modules (quote (autoaway autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring services smiley stamp spelling truncate)))
 '(erc-nick "haxney")
 '(erc-nickserv-alist (quote ((Ars nil nil "Census" "IDENTIFY" nil nil nil) (Austnet "NickOP!service@austnet.org" "/msg\\s-NickOP@austnet.org\\s-identify\\s-<password>" "nickop@austnet.org" "identify" nil nil nil) (Azzurra "NickServ!service@azzurra.org" "/ns\\s-IDENTIFY\\s-password" "NickServ" "IDENTIFY" nil nil nil) (BitlBee nil nil "&bitlbee" "identify" nil nil nil) (BRASnet "NickServ!services@brasnet.org" "/NickServ\\s-IDENTIFY\\s-senha" "NickServ" "IDENTIFY" nil "" nil) (DALnet "NickServ!service@dal.net" "/msg\\s-NickServ@services.dal.net\\s-IDENTIFY\\s-<password>" "NickServ@services.dal.net" "IDENTIFY" nil nil nil) (freenode "NickServ!NickServ@services." "This\\s-nickname\\s-is\\s-registered.\\s-Please\\s-choose" "NickServ" "IDENTIFY" nil nil "You\\s-are\\s-now\\s-identified\\s-for\\s-") (GalaxyNet "NS!nickserv@galaxynet.org" "Please\\s-change\\s-nicks\\s-or\\s-authenticate." "NS@services.galaxynet.org" "AUTH" t nil nil) (GRnet "NickServ!service@irc.gr" "This\\s-nickname\\s-is\\s-registered\\s-and\\s-protected." "NickServ" "IDENTIFY" nil nil "Password\\s-accepted\\s--\\s-you\\s-are\\s-now\\s-recognized.") (iip "Trent@anon.iip" "type\\s-/squery\\s-Trent\\s-identify\\s-<password>" "Trent@anon.iip" "IDENTIFY" nil "SQUERY" nil) (OFTC "NickServ!services@services.oftc.net" nil "NickServ" "IDENTIFY" nil nil "You\\s-are\\s-successfully\\s-identified\\s-as\\s-") (Rizon "NickServ!service@rizon.net" "This\\s-nickname\\s-is\\s-registered\\s-and\\s-protected." "NickServ" "IDENTIFY" nil nil "Password\\s-accepted\\s--\\s-you\\s-are\\s-now\\s-recognized.") (QuakeNet nil nil "Q@CServe.quakenet.org" "auth" t nil nil) (SlashNET "NickServ!services@services.slashnet.org" "/msg\\s-NickServ\\s-IDENTIFY\\s-password" "NickServ@services.slashnet.org" "IDENTIFY" nil nil nil) (EsperNet "NickServ!services@esper.net" "This\\s-nickname\\s-is\\s-registered\\s-and\\s-protected." "NickServ" "IDENTIFY" nil nil "-NickServ-\\sPassword\\saccepted\\s--\\syou\\sare\\snow\\srecognized."))))
 '(erc-pals (quote ("evie" "sinserrar")))
 '(erc-prompt-for-nickserv-password nil)
 '(erc-services-mode t)
 '(erc-stamp-mode t)
 '(erc-timestamp-format "[%R-%m/%d]")
 '(erc-track-exclude-server-buffer t)
 '(erc-track-exclude-types (quote ("JOIN" "NICK" "PART" "QUIT" "MODE" "333" "353")))
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(erc-track-position-in-mode-line t)
 '(erc-track-remove-disconnected-buffers t)
 '(erc-truncate-buffer-on-save t)
 '(erc-truncate-mode t)
 '(erc-user-full-name "Daniel Hackney")
 '(eshell-before-prompt-hook (quote (smart-tab-mode-off)))
 '(eshell-directory-name "~/.emacs.d/tmp/eshell/")
 '(eshell-mode-hook (quote (visual-line-mode)))
 '(espresso-indent-level 4 t)
 '(espresso-js-tmpdir "~/.emacs.d/tmp/js")
 '(fill-column 80)
 '(flymake-allowed-file-name-masks (quote (("\\.c\\'" flymake-simple-make-init) ("\\.cpp\\'" flymake-simple-make-init) ("\\.xml\\'" flymake-xml-init) ("\\.cs\\'" flymake-simple-make-init) ("\\.p[ml]\\'" flymake-perl-init) ("\\.php[345]?\\'" flymake-php-init) ("\\.module\\'" flymake-php-init) ("\\.inc\\'" flymake-php-init) ("\\.install\\'" flymake-php-init) ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup) ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup) ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup) ("\\.tex\\'" flymake-simple-tex-init) ("\\.idl\\'" flymake-simple-make-init))))
 '(flymake-start-syntax-check-on-find-file nil)
 '(flyspell-use-meta-tab nil)
 '(font-use-system-font t)
 '(geben-temporary-file-directory "~/.emacs.d/tmp/geben")
 '(git-append-signed-off-by t)
 '(global-company-mode t)
 '(global-linum-mode t)
 '(global-semantic-highlight-edits-mode t nil (semantic/util-modes))
 '(global-semantic-highlight-func-mode t nil (semantic/util-modes))
 '(global-semantic-idle-scheduler-mode t nil (semantic/idle))
 '(global-semantic-mru-bookmark-mode t)
 '(global-semantic-show-parser-state-mode t nil (semantic/util-modes))
 '(global-semantic-stickyfunc-mode t nil (semantic/util-modes))
 '(global-semanticdb-minor-mode t)
 '(global-smart-tab-mode t)
 '(global-undo-tree-mode t)
 '(highline-priority 1)
 '(hippie-expand-dabbrev-as-symbol nil)
 '(hippie-expand-try-functions-list (quote (try-complete-file-name-partially try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially)))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "_whizzy_.*" ".*\\.raux" ".*\\.wdvi" "\\.git")))
 '(ido-minibuffer-setup-hook (quote ((lambda nil (local-set-key "" (quote ido-prev-match))) (lambda nil (local-set-key "" (quote ido-next-match))))))
 '(ido-mode (quote both) nil (ido))
 '(ido-rotate-file-list-default t)
 '(ido-save-directory-list-file "~/.emacs.d/tmp/.ido.last")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(line-number-mode t)
 '(load-home-init-file t t)
 '(magit-commit-signoff t)
 '(menu-bar-mode nil)
 '(nxhtml-skip-welcome t)
 '(nxml-mode-hook (quote (nxml-enable-unicode-char-name-sets rng-nxml-mode-init turn-on-flyspell)))
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-custom-commands (quote (("p" tags "PROJECT-MAYBE-DONE" nil))))
 '(org-agenda-files (quote ("~/org/root.org")))
 '(org-archive-location "%s_archive::* Archived Tasks")
 '(org-capture-templates (quote (("t" "Todo" entry (file+headline "~/org/root.org" "Task Inbox") "** TODO %?
   %i"))))
 '(org-default-notes-file "~/org/root.org")
 '(org-export-latex-classes (quote (("vita" "\\documentclass[ComputerScience]{vita}
\\usepackage[left=2cm,top=1cm,right=2cm]{geometry}
\\usepackage{multicol}
\\addtolength{\\columnsep}{-0.3in}
\\addtolength{\\multicolsep}{-0.1in}
\\usepackage{savetrees}
\\usepackage[compact]{titlesec}
\\titlespacing{\\section}{0pt}{*0}{*0}
\\titlespacing{\\subsection}{0pt}{*0}{*0}
\\titlespacing{\\subsubsection}{0pt}{*0}{*0}
\\usepackage{comment}
\\usepackage{setspace}
\\singlespacing
\\setlength{\\topsep}{-0.6in}" ("\\section{%s \\hrulefill}" . "\\section*{%s \\hrulefill}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}") ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))) t)
 '(org-id-locations-file "~/.emacs.d/tmp/.org-id-locations")
 '(org-log-done (quote time))
 '(org-modules (quote (org-bibtex org-info org-jsinfo org-habit org-irc org-protocol org-registry org-special-blocks)))
 '(org-registry-file "~/.emacs.d/tmp/org-registry.el")
 '(org-return-follows-link t)
 '(org-todo-keywords (quote ((sequence "TODO" "STARTED" "WAITING" "DONE"))))
 '(org-track-directory "~/.emacs.d/elisp/")
 '(org-track-remove-package t)
 '(package-base "~/Projects/elpa/")
 '(persp-completing-func (quote ido-completing-read))
 '(pgg-cache-passphrase nil)
 '(pgg-default-user-id "A016D1D6")
 '(php-mode-hook (quote (c-enable-electric-state (lambda nil (c-set-style "drupal")))) t)
 '(pop-up-windows nil)
 '(popcmp-group-alternatives nil)
 '(predictive-auto-complete nil)
 '(predictive-auto-learn t)
 '(predictive-dict-autosave-on-kill-buffer nil)
 '(predictive-dict-autosave-on-mode-disable nil)
 '(predictive-main-dict (quote (my-dict dict-english)))
 '(quack-default-program "mzscheme -g")
 '(quack-dir "~/.emacs.d/tmp/quack")
 '(quack-global-menu-p nil)
 '(quack-pretty-lambda-p nil)
 '(quack-run-scheme-always-prompts-p nil)
 '(quack-smart-open-paren-p nil)
 '(recentf-save-file "~/.emacs.d/tmp/recentf")
 '(save-completions-file-name "~/.emacs.d/tmp/completions")
 '(save-place-file "~/.emacs.d/tmp/places")
 '(scroll-bar-mode nil)
 '(semantic-default-submodes (quote (global-semantic-highlight-func-mode global-semantic-decoration-mode global-semantic-stickyfunc-mode global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-summary-mode global-semantic-mru-bookmark-mode)))
 '(semantic-mode t)
 '(semantic-python-dependency-system-include-path (quote ("/usr/local/lib/python2.6/dist-packages/Couchapp-0.3.32-py2.6.egg" "/usr/local/lib/python2.6/dist-packages/paisley-0.1-py2.6.egg" "/usr/local/lib/python2.6/dist-packages/feedparser-4.1-py2.6.egg" "/usr/local/lib/python2.6/dist-packages/python_twitter-0.6-py2.6.egg" "/usr/local/lib/python2.6/dist-packages/twython-0.9-py2.6.egg" "/usr/local/lib/python2.6/dist-packages/tweepy-1.3-py2.6.egg" "/usr/lib/python2.6" "/usr/lib/python2.6/plat-linux2" "/usr/lib/python2.6/lib-tk" "/usr/lib/python2.6/lib-old" "/usr/lib/python2.6/lib-dynload" "/usr/lib/python2.6/dist-packages" "/usr/lib/python2.6/dist-packages/PIL" "/usr/lib/python2.6/dist-packages/gst-0.10" "/usr/lib/pymodules/python2.6" "/usr/lib/python2.6/dist-packages/gtk-2.0" "/usr/lib/pymodules/python2.6/gtk-2.0" "/usr/lib/python2.6/dist-packages/wx-2.8-gtk2-unicode" "/usr/local/lib/python2.6/dist-packages" "/usr/lib/pymodules/python2.6/IPython/Extensions")))
 '(semanticdb-default-save-directory "~/.emacs.d/tmp/semanticdb")
 '(sentence-end-double-space nil)
 '(server-switch-hook (quote (turn-on-flyspell)))
 '(server-visit-hook (quote (longlines-mode-on server-edit-presets)))
 '(show-paren-mode t)
 '(smart-tab-using-hippie-expand t)
 '(smex-save-file "~/.emacs.d/tmp/smex.save")
 '(smiley-data-directory "~/.emacs.d/smileys")
 '(sml-modeline-mode t)
 '(srecode-map-save-file "~/.emacs.d/tmp/srecode-map")
 '(term-mode-hook (quote (visual-line-mode-on)))
 '(text-mode-hook (quote (turn-on-flyspell turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(tramp-completion-reread-directory-timeout 5)
 '(tramp-default-method "ssh")
 '(tramp-persistency-file-name "~/.emacs.d/tmp/tramp")
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(undo-tree-mode-lighter " uT")
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(wdired-use-dired-vertical-movement (quote sometimes))
 '(weblogger-config-alist (quote (("haxney.org" "http://www.blogger.com/api" "dan@haxney.org" "" "7770370347473031286"))))
 '(windmove-wrap-around t)
 '(woman-use-own-frame nil)
 '(x-select-enable-primary t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-face ((((class color) (background light)) (:background "orange1"))) t)
 '(bm-fringe-face ((((class color) (background light)) (:background "orange1"))) t)
 '(bm-fringe-persistent-face ((((class color) (background light)) (:background "DarkBlue"))) t)
 '(bm-persistent-face ((((class color) (background light)) (:background "DarkBlue"))) t)
 '(diff-added ((t (:inherit diff-changed-face :background "Green"))))
 '(diff-added-face ((t (:inherit diff-changed-face :background "Green"))) t)
 '(diff-removed ((t (:inherit diff-changed-face :background "red"))))
 '(diff-removed-face ((t (:inherit diff-changed-face :background "red"))) t)
 '(flyspell-duplicate ((((class color)) (:foreground "Gold3" :underline t :weight bold))))
 '(flyspell-incorrect ((((class color)) (:foreground "magenta" :underline t :weight bold))))
 '(whizzy-point-face ((((class color)) nil)) t))
