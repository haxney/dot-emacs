(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-breadcrumbs-in-header-flag nil)
 '(Info-breadcrumbs-in-mode-line-mode t)
 '(Info-mode-hook '(flyspell-mode-off scroll-lock-mode))
 '(LaTeX-command "latex -shell-escape")
 '(LaTeX-mode-hook '(LaTeX-preview-setup flymake-mode-off turn-on-flyspell))
 '(TeX-master 'dwim)
 '(ac-auto-show-menu t)
 '(ac-auto-start t)
 '(ac-dictionary-directories '(".emacs.d/elpa/auto-complete-1.4.20110207/dict"))
 '(ac-dictionary-files '("~/.dict" "~/.hunspell_en_US"))
 '(ac-ignore-case nil)
 '(ac-menu-height 15)
 '(ac-trigger-commands '(self-insert-command org-self-insert-command))
 '(ac-use-menu-map t)
 '(after-save-hook '(executable-make-buffer-file-executable-if-script-p))
 '(before-save-hook '(whitespace-cleanup))
 '(blink-cursor-mode nil)
 '(bm-annotate-on-create nil)
 '(bm-buffer-persistence t)
 '(bm-highlight-style 'bm-highlight-only-fringe)
 '(bm-recenter t)
 '(bm-restore-repository-on-load t t)
 '(bookmark-save-flag 2)
 '(case-fold-search t)
 '(coffee-js-mode 'javascript-mode)
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(comint-history-isearch 'dwim)
 '(comint-input-ignoredups t)
 '(comint-process-echoes t)
 '(comint-scroll-show-maximum-output nil)
 '(company-backends
   '(company-elisp company-nxml company-css company-semantic
                   (company-etags company-keywords company-dabbrev-code)
                   company-dabbrev company-oddmuse company-files))
 '(company-begin-commands '(self-insert-command org-self-insert-command))
 '(company-idle-delay t)
 '(company-major-modes
   '(css-mode emacs-lisp-mode nxml-mode lisp-interaction-mode org-mode log-edit-mode))
 '(confirm-kill-emacs 'yes-or-no-p)
 '(css-indent-offset 2)
 '(cua-delete-selection nil)
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(current-language-environment "UTF-8")
 '(custom-raised-buttons t)
 '(custom-safe-themes
   '("45631691477ddee3df12013e718689dafa607771e7fd37ebc6c6eb9529a8ede5" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default))
 '(default-frame-alist '((cursor-type bar . 2) (font . "DejaVu Sans Mono-8")))
 '(default-input-method "rfc1345")
 '(default-major-mode 'text-mode t)
 '(desktop-file-name-format 'tilde)
 '(desktop-load-locked-desktop t)
 '(desktop-save t)
 '(desktop-save-mode t)
 '(dired-details-hidden-string "[...]  ")
 '(dired-listing-switches "-alh --group-directories-first")
 '(dired-omit-files "^\\.?#\\|^\\..*$")
 '(display-line-numbers-minor-tick 10)
 '(display-line-numbers-widen t)
 '(doc-view-resolution 200)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(edit-server-new-frame nil)
 '(editorconfig-mode t)
 '(elpy-default-minor-modes '(eldoc-mode flycheck-mode auto-complete-mode))
 '(emacs-lisp-mode-hook '(turn-on-eldoc-mode flyspell-prog-mode paredit-mode))
 '(enable-recursive-minibuffers t)
 '(erc-autoaway-mode t)
 '(erc-autojoin-channels-alist '(("freenode.net" "#emacs") ("quadium.net" "#betaspring")))
 '(erc-autojoin-mode t)
 '(erc-email-userid "dan@haxney.org")
 '(erc-generate-log-file-name-function 'erc-generate-log-file-name-date-and-name)
 '(erc-interpret-mirc-color t)
 '(erc-log-insert-log-on-open nil)
 '(erc-log-mode t)
 '(erc-log-write-after-insert t)
 '(erc-log-write-after-send t)
 '(erc-max-buffer-size 2000000)
 '(erc-modules
   '(autoaway autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands notify readonly ring services smiley stamp truncate))
 '(erc-move-to-prompt-mode t)
 '(erc-netsplit-mode t)
 '(erc-networks-mode t)
 '(erc-nick "haxney")
 '(erc-nick-changed-functions '(erc-nickserv-identify-on-nick-change))
 '(erc-nickserv-alist
   '((Ars nil nil "Census" "IDENTIFY" nil nil nil)
     (Austnet "NickOP!service@austnet.org" "/msg\\s-NickOP@austnet.org\\s-identify\\s-<password>" "nickop@austnet.org" "identify" nil nil nil)
     (Azzurra "NickServ!service@azzurra.org" "\2/ns\\s-IDENTIFY\\s-password\2" "NickServ" "IDENTIFY" nil nil nil)
     (BitlBee nil nil "&bitlbee" "identify" nil nil nil)
     (BRASnet "NickServ!services@brasnet.org" "\2/NickServ\\s-IDENTIFY\\s-\37senha\37\2" "NickServ" "IDENTIFY" nil "" nil)
     (DALnet "NickServ!service@dal.net" "/msg\\s-NickServ@services.dal.net\\s-IDENTIFY\\s-<password>" "NickServ@services.dal.net" "IDENTIFY" nil nil nil)
     (freenode "NickServ!NickServ@services." "This\\s-nickname\\s-is\\s-registered.\\s-Please\\s-choose" "NickServ" "IDENTIFY" nil nil "You\\s-are\\s-now\\s-identified\\s-for\\s-")
     (GalaxyNet "NS!nickserv@galaxynet.org" "Please\\s-change\\s-nicks\\s-or\\s-authenticate." "NS@services.galaxynet.org" "AUTH" t nil nil)
     (GRnet "NickServ!service@irc.gr" "This\\s-nickname\\s-is\\s-registered\\s-and\\s-protected." "NickServ" "IDENTIFY" nil nil "Password\\s-accepted\\s--\\s-you\\s-are\\s-now\\s-recognized.")
     (iip "Trent@anon.iip" "type\\s-/squery\\s-Trent\\s-identify\\s-<password>" "Trent@anon.iip" "IDENTIFY" nil "SQUERY" nil)
     (Libera.Chat "NickServ!NickServ@services.libera.chat" "This\\s-nickname\\s-is\\s-registered.\\s-Please\\s-choose" "NickServ" "IDENTIFY" nil nil "You\\s-are\\s-now\\s-identified\\s-for\\s-")
     (OFTC "NickServ!services@services.oftc.net" nil "NickServ" "IDENTIFY" nil nil "You\\s-are\\s-successfully\\s-identified\\s-as\\s-\2")
     (Rizon "NickServ!service@rizon.net" "This\\s-nickname\\s-is\\s-registered\\s-and\\s-protected." "NickServ" "IDENTIFY" nil nil "Password\\s-accepted\\s--\\s-you\\s-are\\s-now\\s-recognized.")
     (QuakeNet nil nil "Q@CServe.quakenet.org" "auth" t nil nil)
     (SlashNET "NickServ!services@services.slashnet.org" "/msg\\s-NickServ\\s-IDENTIFY\\s-\37password" "NickServ@services.slashnet.org" "IDENTIFY" nil nil nil)
     (EsperNet "NickServ!services@esper.net" "This\\s-nickname\\s-is\\s-registered\\s-and\\s-protected." "NickServ" "IDENTIFY" nil nil "-NickServ-\\sPassword\\saccepted\\s--\\syou\\sare\\snow\\srecognized.")))
 '(erc-nickserv-identified-hook '(erc-autojoin-after-ident))
 '(erc-notifications-mode t)
 '(erc-prompt 'erc-custom-prompt)
 '(erc-prompt-for-nickserv-password nil)
 '(erc-reuse-buffers t)
 '(erc-server "localhost")
 '(erc-services-mode t)
 '(erc-spelling-mode t)
 '(erc-stamp-mode t)
 '(erc-timestamp-format "[%R-%m/%d]")
 '(erc-track-exclude-server-buffer t)
 '(erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "333" "353"))
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(erc-track-position-in-mode-line t t)
 '(erc-track-remove-disconnected-buffers t)
 '(erc-truncate-buffer-on-save t)
 '(erc-truncate-mode t)
 '(erc-user-full-name "Daniel Hackney")
 '(eshell-mode-hook '(visual-line-mode) t)
 '(ess-S-assign "_")
 '(ess-help-mode-hook '(scroll-lock-mode))
 '(ess-language "R" t)
 '(ess-style 'RRR)
 '(fill-column 80)
 '(find-file-hook
   '(global-subword-mode-check-buffers global-undo-tree-mode-check-buffers recentf-track-opened-file undo-tree-load-history-hook mode-local-post-major-mode-change global-font-lock-mode-check-buffers epa-file-find-file-hook save-place-find-file-hook))
 '(flycheck-flake8-maximum-line-length 80)
 '(flymake-allowed-file-name-masks
   '(("\\.c\\'" flymake-simple-make-init)
     ("\\.cpp\\'" flymake-simple-make-init)
     ("\\.xml\\'" flymake-xml-init)
     ("\\.cs\\'" flymake-simple-make-init)
     ("\\.p[ml]\\'" flymake-perl-init)
     ("\\.php[345]?\\'" flymake-php-init)
     ("\\.module\\'" flymake-php-init)
     ("\\.inc\\'" flymake-php-init)
     ("\\.install\\'" flymake-php-init)
     ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
     ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
     ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
     ("\\.tex\\'" flymake-simple-tex-init)
     ("\\.idl\\'" flymake-simple-make-init)))
 '(flymake-proc-allowed-file-name-masks
   '(("\\.c\\'" flymake-simple-make-init)
     ("\\.cpp\\'" flymake-simple-make-init)
     ("\\.xml\\'" flymake-xml-init)
     ("\\.cs\\'" flymake-simple-make-init)
     ("\\.p[ml]\\'" flymake-perl-init)
     ("\\.php[345]?\\'" flymake-php-init)
     ("\\.module\\'" flymake-php-init)
     ("\\.inc\\'" flymake-php-init)
     ("\\.install\\'" flymake-php-init)
     ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
     ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
     ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
     ("\\.tex\\'" flymake-simple-tex-init)
     ("\\.idl\\'" flymake-simple-make-init)))
 '(flymake-start-on-flymake-mode nil)
 '(flymake-start-syntax-check-on-find-file nil)
 '(flyspell-use-meta-tab nil)
 '(geben-dbgp-redirect-buffer-init-hook '(view-mode))
 '(geiser-default-implementation 'racket)
 '(geiser-racket-extra-keywords
   '("define-type-alias" "define-syntax-rule" "provide" "require" "unless" "when" "with-handlers" "define-type" "type-case"))
 '(global-flycheck-mode t)
 '(global-hl-line-mode t)
 '(global-hl-line-sticky-flag t)
 '(global-rinari-mode t)
 '(global-smart-tab-mode t)
 '(global-subword-mode t)
 '(global-undo-tree-mode t)
 '(graphviz-dot-indent-width 4)
 '(helm-allow-skipping-current-buffer t)
 '(helm-buffer-max-length 40)
 '(helm-c-adaptive-history-length 1000)
 '(helm-c-enable-eval-defun-hack nil)
 '(helm-c-use-adaptative-sorting t)
 '(helm-descbinds-mode t)
 '(helm-idle-delay 0.05)
 '(helm-input-idle-delay 0.05)
 '(hippie-expand-dabbrev-as-symbol t)
 '(hippie-expand-try-functions-list
   '(try-complete-lisp-symbol-partially try-complete-file-name-partially try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill))
 '(history-delete-duplicates t)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-ignore-files
   '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "_whizzy_.*" ".*\\.raux" ".*\\.wdvi" "\\.git"))
 '(ido-minibuffer-setup-hook
   '((lambda nil
       (local-set-key "\20" 'ido-prev-match))
     (lambda nil
       (local-set-key "\16" 'ido-next-match))))
 '(ido-mode 'both nil (ido))
 '(ido-rotate-file-list-default t)
 '(image-dired-cmd-create-temp-image-options "%p -size %wx%h \"%f\" -resize \"%wx%h>\" jpeg:\"%t\"")
 '(image-dired-cmd-create-thumbnail-options "%p -size %wx%h \"%f\" -resize \"%wx%h>\" jpeg:\"%t\"")
 '(indent-tabs-mode nil)
 '(inferior-ess-exit-command "q(save=\"no\")\12" t)
 '(inhibit-startup-screen t)
 '(ispell-program-name "aspell")
 '(js-indent-level 2)
 '(js2-global-externs '("WebSocket" "$"))
 '(js2-include-node-externs t)
 '(js2-strict-missing-semi-warning nil)
 '(keyfreq-autosave-mode t)
 '(keyfreq-mode t)
 '(kill-do-not-save-duplicates t)
 '(line-number-mode t)
 '(linum-delay t)
 '(load-dir-ignore-errors t)
 '(load-dirs t)
 '(load-home-init-file t t)
 '(ls-lisp-dirs-first t)
 '(lsp-file-watch-ignored-directories
   '("[/\\\\]\\.git\\'" "[/\\\\]\\.hg\\'" "[/\\\\]\\.bzr\\'" "[/\\\\]_darcs\\'" "[/\\\\]\\.svn\\'" "[/\\\\]_FOSSIL_\\'" "[/\\\\]\\.idea\\'" "[/\\\\]\\.ensime_cache\\'" "[/\\\\]\\.eunit\\'" "[/\\\\]node_modules" "[/\\\\]\\.fslckout\\'" "[/\\\\]\\.tox\\'" "[/\\\\]dist\\'" "[/\\\\]dist-newstyle\\'" "[/\\\\]\\.stack-work\\'" "[/\\\\]\\.bloop\\'" "[/\\\\]\\.metals\\'" "[/\\\\]target\\'" "[/\\\\]\\.ccls-cache\\'" "[/\\\\]\\.vscode\\'" "[/\\\\]\\.deps\\'" "[/\\\\]build-aux\\'" "[/\\\\]autom4te.cache\\'" "[/\\\\]\\.reference\\'" "[/\\\\]bin/Debug\\'" "[/\\\\]obj\\'" "[/\\\\].pio\\'"))
 '(magit-push-always-verify nil)
 '(menu-bar-mode nil)
 '(minibuffer-prompt-properties
   '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
 '(mumamo-noweb2-mode-from-ext '(("php" . php+-mode) ("c" . c-mode)))
 '(nxhtml-skip-welcome t)
 '(nxml-mode-hook
   '(nxml-enable-unicode-char-name-sets rng-nxml-mode-init turn-on-flyspell) t)
 '(nxml-slash-auto-complete-flag t)
 '(occur-mode-hook '(turn-on-font-lock turn-off-flyspell))
 '(org-agenda-files '("~/org/root.org"))
 '(org-archive-location "~/org/archive.org::* From %s")
 '(org-archive-mark-done t)
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (R . t)
     (dot . t)
     (calc . t)
     (plantuml . t)))
 '(org-capture-templates
   '(("t" "Todo" entry
      (file+headline "~/org/root.org" "Task Inbox")
      "** TODO %?\12   %i")
     ("d" "Day Page" entry #'org-open-day-page "\12* Sleep" :immediate-finish t :unnarrowed t)))
 '(org-clock-in-resume t)
 '(org-clock-into-drawer t)
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-persist 'clock)
 '(org-confirm-babel-evaluate nil)
 '(org-cycle-include-plain-lists nil)
 '(org-default-notes-file "~/org/root.org")
 '(org-edit-src-persistent-message nil)
 '(org-entities-user
   '(("ominus" "\\ominus" t "&ominus;" "[circled minus]" "[circled minus]" "⊖")))
 '(org-export-with-smart-quotes t)
 '(org-insert-heading-respect-content t)
 '(org-latex-classes
   '(("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("vita" "\\documentclass[ComputerScience,10pt]{vita}"
      ("\\section{%s \\hrulefill}" . "\\section*{%s \\hrulefill}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
 '(org-latex-packages-alist '(("" "listings" nil) ("" "color" nil) ("" "minted" nil)))
 '(org-latex-pdf-process
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
 '(org-latex-src-block-backend 'minted)
 '(org-list-demote-modify-bullet '(("-" . "+")))
 '(org-list-description-max-indent 10)
 '(org-log-done 'time)
 '(org-mode-hook '(flyspell-mode))
 '(org-modules
   '(ol-bibtex org-habit ol-info ol-irc org-protocol org-tempo))
 '(org-roam-directory "~/org")
 '(org-special-ctrl-a/e t)
 '(org-src-fontify-natively t)
 '(org-src-lang-modes
   '(("ocaml" . tuareg)
     ("elisp" . emacs-lisp)
     ("ditaa" . artist)
     ("asymptote" . asy)
     ("dot" . graphviz-dot)
     ("sqlite" . sql)
     ("calc" . fundamental)
     ("C" . c)
     ("cpp" . c++)
     ("C++" . c++)
     ("screen" . shell-script)
     ("shell" . sh)
     ("bash" . sh)))
 '(org-src-window-setup 'other-window)
 '(org-startup-folded t)
 '(org-todo-keywords '((sequence "TODO" "STARTED" "WAITING" "DONE")))
 '(org-use-sub-superscripts '{})
 '(package-archive-upload-base "~/Dropbox/Public/elpa-haxney")
 '(package-base "~/Projects/elpa/")
 '(package-selected-packages
   '(paredit js2-mode graphviz-dot-mode jedi yaml-mode websocket vlf sws-mode smooth-scrolling restclient mediawiki inflections iedit flycheck auto-complete feature-mode editorconfig sass-mode gh snap-indent delight dired+ comint newcomment abbrev dired clang-format company-lsp lsp-ui ccls platformio-mode ess magit image+ use-package smart-mode-line ruby-test-mode yard-mode multiple-cursors geiser geben elpy protobuf-mode gradle-mode window-margin whitespace-cleanup-mode vimgolf vbnet-mode undo-tree unbound typing toml-mode tidy smex scss-mode rvm rustfmt ruby-block rspec-mode request racer quack psl-mode php+-mode org-table-comment org-magit nose nlinum mode-compile mocker memory-usage lua-mode load-dir list-utils less keywiz keyfreq jade-mode idomenu htmlize hl-sexp hl-line+ helm-git helm-descbinds helm-ack helm-R guide-key groovy-mode gnuplot gist fuzzy furl full-ack erc-nick-notify dired-details csv-mode coffee-mode cargo auto-indent-mode auto-compile auctex apache-mode ace-jump-mode))
 '(persp-completing-func 'ido-completing-read)
 '(pgg-cache-passphrase nil)
 '(pgg-default-user-id "A016D1D6")
 '(php-basic-offset 2)
 '(php-file-patterns
   '("\\.php[s345t]?\\'" "\\.phtml\\'" "\\.inc\\'" "\\.install\\'" "\\.module\\'"))
 '(php-mode-coding-style 'drupal)
 '(php-mode-hook '(c-style-drupal))
 '(pop-up-windows nil)
 '(popcmp-group-alternatives nil)
 '(predictive-auto-complete nil)
 '(predictive-auto-learn t)
 '(predictive-dict-autosave-on-kill-buffer nil)
 '(predictive-dict-autosave-on-mode-disable nil)
 '(predictive-main-dict '(my-dict dict-english))
 '(psl-program-name "psl-interp")
 '(python-fill-docstring-style 'django)
 '(python-python-command "python3")
 '(quack-global-menu-p nil)
 '(quack-pltish-keywords-to-fontify
   '("and" "begin" "begin0" "c-declare" "c-lambda" "case" "case-lambda" "class" "class*" "class*/names" "class100" "class100*" "compound-unit/sig" "cond" "cond-expand" "define" "define-class" "define-compound-unit" "define-const-structure" "define-constant" "define-embedded" "define-entry-point" "define-external" "define-for-syntax" "define-foreign-record" "define-foreign-type" "define-foreign-variable" "define-generic" "define-generic-procedure" "define-inline" "define-location" "define-macro" "define-method" "define-module" "define-opt" "define-public" "define-reader-ctor" "define-record" "define-record-printer" "define-record-type" "define-signature" "define-struct" "define-structure" "define-syntax" "define-syntax-set" "define-values" "define-values-for-syntax" "define-values/invoke-unit/infer" "define-values/invoke-unit/sig" "define/contract" "define/override" "define/private" "define/public" "define/kw" "delay" "do" "else" "exit-handler" "field" "if" "import" "inherit" "inherit-field" "init" "init-field" "init-rest" "instantiate" "interface" "lambda" "lambda/kw" "let" "let*" "let*-values" "let+" "let-syntax" "let-values" "let/ec" "letrec" "letrec-values" "letrec-syntax" "match-lambda" "match-lambda*" "match-let" "match-let*" "match-letrec" "match-define" "mixin" "module" "opt-lambda" "or" "override" "override*" "namespace-variable-bind/invoke-unit/sig" "parameterize" "parameterize*" "parameterize-break" "private" "private*" "protect" "provide" "provide-signature-elements" "provide/contract" "public" "public*" "quasiquote" "quasisyntax" "quasisyntax/loc" "quote" "receive" "rename" "require" "require-for-syntax" "send" "send*" "set!" "set!-values" "signature->symbols" "super-instantiate" "syntax" "syntax/loc" "syntax-case" "syntax-case*" "syntax-error" "syntax-rules" "unit/sig" "unless" "unquote" "unquote-splicing" "when" "with-handlers" "with-method" "with-syntax" "define-type-alias" "define-type" "define-struct:" "define:" "let:" "letrec:" "let*:" "lambda:" "plambda:" "case-lambda:" "pcase-lambda:" "require/typed" "require/opaque-type" "require-typed-struct" "inst" "ann"))
 '(quack-pretty-lambda-p nil)
 '(quack-run-scheme-always-prompts-p nil)
 '(quack-smart-open-paren-p nil)
 '(recentf-mode t)
 '(require-final-newline t)
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values
   '((require-final-newline . t)
     (buffer-file-coding-system . utf-8-unix)
     (whitespace-style face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark)
     (outline-minor-mode)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby")
     (lexical-binding . t)))
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-limit 1000)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-error-top-bottom t)
 '(scss-compile-at-save nil)
 '(select-active-regions nil)
 '(select-enable-primary t)
 '(semantic-default-submodes
   '(global-semantic-highlight-func-mode global-semantic-decoration-mode global-semantic-stickyfunc-mode global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-summary-mode global-semantic-mru-bookmark-mode))
 '(semantic-mode t)
 '(semantic-python-dependency-system-include-path
   '("/usr/local/lib/python2.6/dist-packages/Couchapp-0.3.32-py2.6.egg" "/usr/local/lib/python2.6/dist-packages/paisley-0.1-py2.6.egg" "/usr/local/lib/python2.6/dist-packages/feedparser-4.1-py2.6.egg" "/usr/local/lib/python2.6/dist-packages/python_twitter-0.6-py2.6.egg" "/usr/local/lib/python2.6/dist-packages/twython-0.9-py2.6.egg" "/usr/local/lib/python2.6/dist-packages/tweepy-1.3-py2.6.egg" "/usr/lib/python2.6" "/usr/lib/python2.6/plat-linux2" "/usr/lib/python2.6/lib-tk" "/usr/lib/python2.6/lib-old" "/usr/lib/python2.6/lib-dynload" "/usr/lib/python2.6/dist-packages" "/usr/lib/python2.6/dist-packages/PIL" "/usr/lib/python2.6/dist-packages/gst-0.10" "/usr/lib/pymodules/python2.6" "/usr/lib/python2.6/dist-packages/gtk-2.0" "/usr/lib/pymodules/python2.6/gtk-2.0" "/usr/lib/python2.6/dist-packages/wx-2.8-gtk2-unicode" "/usr/local/lib/python2.6/dist-packages" "/usr/lib/pymodules/python2.6/IPython/Extensions"))
 '(send-mail-function 'mailclient-send-it)
 '(sentence-end-double-space nil)
 '(server-visit-hook '(server-edit-presets))
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(smart-tab-using-hippie-expand t)
 '(sml/replacer-regexp-list
   '(("/gpfs/main/home/dhackney/" "~/")
     ("^~/\\.emacs\\.d/" ":ED:")
     ("^/sudo:.*:" ":SU:")
     ("^~/Documents/" ":Doc:")
     ("^~/Dropbox/" ":DB:")
     ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
     ("^~/[Gg]it/" ":Git:")
     ("^~/[Gg]it[Hh]ub/" ":Git:")
     ("^~/[Gg]it-?[Pp]rojects/" ":Git:")
     ("^~/work/classes/" ":Classes:")
     ("^~/work/railskating/" ":Rs:")))
 '(sml/theme 'light)
 '(split-width-threshold nil)
 '(text-mode-hook '(turn-on-flyspell turn-on-auto-fill))
 '(tool-bar-mode nil)
 '(tramp-completion-reread-directory-timeout 5)
 '(tramp-default-method "ssh")
 '(tramp-gvfs-methods '("dav" "davs" "obex" "synce" "smb" "ftp" "sftp"))
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(undo-tree-mode-lighter "")
 '(uniquify-buffer-name-style 'post-forward-angle-brackets nil (uniquify))
 '(url-automatic-caching t)
 '(use-package-always-defer t)
 '(use-package-always-ensure t)
 '(vc-display-status nil)
 '(vc-handled-backends nil)
 '(view-mode-hook '(scroll-lock-mode))
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp)))
 '(wdired-use-dired-vertical-movement 'sometimes)
 '(weblogger-config-alist
   '(("haxney.org" "http://www.blogger.com/api" "dan@haxney.org" "" "7770370347473031286")))
 '(whitespace-style
   '(face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark))
 '(windmove-wrap-around t)
 '(woman-imenu t)
 '(woman-use-own-frame nil)
 '(yaml-mode-hook '(yaml-set-imenu-generic-expression flyspell-mode-off)))
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
 '(diff-removed ((t (:inherit diff-changed-face :background "red"))))
 '(flyspell-duplicate ((((class color)) (:foreground "Gold3" :underline t :weight bold))))
 '(flyspell-incorrect ((((class color)) (:foreground "magenta" :underline t :weight bold))))
 '(helm-selection ((t (:inherit highlight))))
 '(helm-source-header ((t (:background "pale goldenrod" :underline t :weight semi-bold))))
 '(highline-face ((t (:background "pale turquoise"))) t)
 '(hl-line ((t (:background "pale turquoise"))))
 '(hl-sexp-face ((t (:background "white smoke"))) t)
 '(magit-item-highlight ((t nil)))
 '(whizzy-point-face ((((class color)) nil)) t))

(provide 'my-custom-values)
;;; my-custom-values.el ends here
