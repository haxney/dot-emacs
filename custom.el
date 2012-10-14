(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-breadcrumbs-in-header-flag nil)
 '(Info-breadcrumbs-in-mode-line-mode t)
 '(Info-mode-hook (quote (flyspell-mode-off scroll-lock-mode less-minor-mode linum-off)))
 '(LaTeX-command "latex -shell-escape")
 '(LaTeX-mode-hook (quote (LaTeX-preview-setup flymake-mode-off turn-on-flyspell)))
 '(TeX-master (quote dwim))
 '(abbrev-file-name "~/.emacs.d/tmp/abbrev_defs")
 '(ac-auto-show-menu t)
 '(ac-auto-start t)
 '(ac-comphist-file "~/.emacs.d/tmp/ac-comphist.dat")
 '(ac-dictionary-directories (quote (".emacs.d/elpa/auto-complete-1.4.20110207/dict")))
 '(after-save-hook (quote (executable-make-buffer-file-executable-if-script-p)))
 '(auto-save-list-file-prefix "~/.emacs.d/tmp/auto-save-list/.saves-")
 '(backup-directory-alist (quote (("." . "~/.emacs.d/tmp/baks/"))))
 '(before-save-hook (quote (whitespace-cleanup)))
 '(blink-cursor-mode nil)
 '(bm-annotate-on-create nil)
 '(bm-buffer-persistence t)
 '(bm-highlight-style (quote bm-highlight-only-fringe))
 '(bm-recenter t)
 '(bm-repository-file "~/.emacs.d/tmp/bm-repository")
 '(bm-restore-repository-on-load t t)
 '(bookmark-default-file "~/.emacs.d/tmp/bookmark")
 '(bookmark-save-flag 2)
 '(case-fold-search t)
 '(coffee-js-mode (quote javascript-mode))
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(comint-history-isearch (quote dwim))
 '(comint-input-ignoredups t)
 '(comint-scroll-show-maximum-output nil)
 '(command-frequency-table-file "~/.emacs.d/tmp/frequencies")
 '(company-backends (quote (company-elisp company-nxml company-css company-semantic (company-etags company-keywords company-dabbrev-code) company-dabbrev company-oddmuse company-files)))
 '(company-begin-commands (quote (self-insert-command org-self-insert-command)))
 '(company-idle-delay t)
 '(company-major-modes (quote (css-mode emacs-lisp-mode nxml-mode lisp-interaction-mode org-mode log-edit-mode)))
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(css-indent-offset 2)
 '(cua-delete-selection nil)
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(current-language-environment "UTF-8")
 '(custom-raised-buttons t)
 '(debug-on-error nil)
 '(default-frame-alist (quote ((cursor-type bar . 2) (font . "DejaVu Sans Mono-8"))))
 '(default-input-method "rfc1345")
 '(default-major-mode (quote text-mode) t)
 '(desktop-file-name-format (quote tilde))
 '(desktop-load-locked-desktop t)
 '(desktop-path (quote ("~/.emacs.d/tmp/")))
 '(desktop-save-mode t)
 '(dired-listing-switches "-alh --group-directories-first")
 '(dired-omit-files "^\\.?#\\|^\\..*$")
 '(ecb-layout-name "left8")
 '(ecb-layout-window-sizes (quote (("left8" (0.22137404580152673 . 0.28125) (0.22137404580152673 . 0.25) (0.22137404580152673 . 0.265625) (0.22137404580152673 . 0.1875)))))
 '(ecb-options-version "2.32")
 '(ecb-tip-of-the-day nil)
 '(ede-project-placeholder-cache-file "~/.emacs.d/tmp/projects.ede")
 '(edit-server-new-frame nil)
 '(emacs-lisp-mode-hook (quote (turn-on-eldoc-mode ert--activate-font-lock-keywords flyspell-prog-mode paredit-mode set-elisp-mode-name)))
 '(enable-recursive-minibuffers t)
 '(erc-autoaway-mode t)
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#emacs") ("quadium.net" "#betaspring"))))
 '(erc-autojoin-mode t)
 '(erc-email-userid "dan@haxney.org")
 '(erc-generate-log-file-name-function (quote erc-generate-log-file-name-date-and-name))
 '(erc-interpret-mirc-color t)
 '(erc-log-channels-directory "~/logs")
 '(erc-log-insert-log-on-open nil)
 '(erc-log-mode t)
 '(erc-log-write-after-insert t)
 '(erc-log-write-after-send t)
 '(erc-max-buffer-size 2000000)
 '(erc-modules (quote (autoaway autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands notify readonly ring services smiley stamp track truncate)))
 '(erc-move-to-prompt-mode t)
 '(erc-netsplit-mode t)
 '(erc-networks-mode t)
 '(erc-nick "haxney")
 '(erc-nick-changed-functions (quote (erc-nickserv-identify-on-nick-change)))
 '(erc-nickserv-alist (quote ((Ars nil nil "Census" "IDENTIFY" nil nil nil) (Austnet "NickOP!service@austnet.org" "/msg\\s-NickOP@austnet.org\\s-identify\\s-<password>" "nickop@austnet.org" "identify" nil nil nil) (Azzurra "NickServ!service@azzurra.org" "/ns\\s-IDENTIFY\\s-password" "NickServ" "IDENTIFY" nil nil nil) (BitlBee nil nil "&bitlbee" "identify" nil nil nil) (BRASnet "NickServ!services@brasnet.org" "/NickServ\\s-IDENTIFY\\s-senha" "NickServ" "IDENTIFY" nil "" nil) (DALnet "NickServ!service@dal.net" "/msg\\s-NickServ@services.dal.net\\s-IDENTIFY\\s-<password>" "NickServ@services.dal.net" "IDENTIFY" nil nil nil) (freenode "NickServ!NickServ@services." "This\\s-nickname\\s-is\\s-registered.\\s-Please\\s-choose" "NickServ" "IDENTIFY" nil nil "You\\s-are\\s-now\\s-identified\\s-for\\s-") (GalaxyNet "NS!nickserv@galaxynet.org" "Please\\s-change\\s-nicks\\s-or\\s-authenticate." "NS@services.galaxynet.org" "AUTH" t nil nil) (GRnet "NickServ!service@irc.gr" "This\\s-nickname\\s-is\\s-registered\\s-and\\s-protected." "NickServ" "IDENTIFY" nil nil "Password\\s-accepted\\s--\\s-you\\s-are\\s-now\\s-recognized.") (iip "Trent@anon.iip" "type\\s-/squery\\s-Trent\\s-identify\\s-<password>" "Trent@anon.iip" "IDENTIFY" nil "SQUERY" nil) (OFTC "NickServ!services@services.oftc.net" nil "NickServ" "IDENTIFY" nil nil "You\\s-are\\s-successfully\\s-identified\\s-as\\s-") (Rizon "NickServ!service@rizon.net" "This\\s-nickname\\s-is\\s-registered\\s-and\\s-protected." "NickServ" "IDENTIFY" nil nil "Password\\s-accepted\\s--\\s-you\\s-are\\s-now\\s-recognized.") (QuakeNet nil nil "Q@CServe.quakenet.org" "auth" t nil nil) (SlashNET "NickServ!services@services.slashnet.org" "/msg\\s-NickServ\\s-IDENTIFY\\s-password" "NickServ@services.slashnet.org" "IDENTIFY" nil nil nil) (EsperNet "NickServ!services@esper.net" "This\\s-nickname\\s-is\\s-registered\\s-and\\s-protected." "NickServ" "IDENTIFY" nil nil "-NickServ-\\sPassword\\saccepted\\s--\\syou\\sare\\snow\\srecognized."))))
 '(erc-nickserv-identified-hook (quote (erc-autojoin-after-ident)))
 '(erc-prompt (quote erc-custom-prompt))
 '(erc-prompt-for-nickserv-password nil)
 '(erc-reuse-buffers t)
 '(erc-server "localhost")
 '(erc-services-mode t)
 '(erc-spelling-mode t)
 '(erc-stamp-mode t)
 '(erc-text-matched-hook (quote (erc-log-matches erc-respond-once-if-away notify-erc)))
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
 '(eshell-directory-name "~/.emacs.d/tmp/eshell/")
 '(eshell-mode-hook (quote (visual-line-mode)))
 '(espresso-indent-level 4 t)
 '(espresso-js-tmpdir "~/.emacs.d/tmp/js")
 '(ess-S-assign "_")
 '(ess-default-style (quote C++))
 '(ess-help-mode-hook (quote (less-minor-mode scroll-lock-mode)))
 '(ess-language "R")
 '(fill-column 80)
 '(find-file-hook (quote (global-subword-mode-check-buffers global-undo-tree-mode-check-buffers recentf-track-opened-file undo-tree-load-history-hook rinari-launch mode-local-post-major-mode-change global-font-lock-mode-check-buffers epa-file-find-file-hook save-place-find-file-hook which-func-ff-hook)))
 '(flymake-allowed-file-name-masks (quote (("\\.c\\'" flymake-simple-make-init) ("\\.cpp\\'" flymake-simple-make-init) ("\\.xml\\'" flymake-xml-init) ("\\.cs\\'" flymake-simple-make-init) ("\\.p[ml]\\'" flymake-perl-init) ("\\.php[345]?\\'" flymake-php-init) ("\\.module\\'" flymake-php-init) ("\\.inc\\'" flymake-php-init) ("\\.install\\'" flymake-php-init) ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup) ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup) ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup) ("\\.tex\\'" flymake-simple-tex-init) ("\\.idl\\'" flymake-simple-make-init))))
 '(flymake-start-syntax-check-on-find-file nil)
 '(flyspell-use-meta-tab nil)
 '(font-use-system-font t)
 '(geben-dbgp-redirect-buffer-init-hook (quote (view-mode)))
 '(geben-temporary-file-directory "~/.emacs.d/tmp/geben")
 '(geiser-default-implementation (quote racket))
 '(geiser-racket-extra-keywords (quote ("define-syntax-rule" "provide" "require" "unless" "when" "with-handlers" "define-type")))
 '(git-append-signed-off-by t)
 '(global-hl-line-mode t)
 '(global-hl-line-sticky-flag t)
 '(global-smart-tab-mode t)
 '(global-subword-mode t)
 '(global-undo-tree-mode t)
 '(helm-allow-skipping-current-buffer t)
 '(helm-buffer-max-length 40)
 '(helm-c-adaptive-history-file "~/.emacs.d/tmp/helm-c-adaptive-history")
 '(helm-c-adaptive-history-length 1000)
 '(helm-c-enable-eval-defun-hack nil)
 '(helm-c-use-adaptative-sorting t)
 '(hippie-expand-dabbrev-as-symbol t)
 '(hippie-expand-try-functions-list (quote (try-complete-lisp-symbol-partially try-complete-file-name-partially try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill)))
 '(history-delete-duplicates t)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "_whizzy_.*" ".*\\.raux" ".*\\.wdvi" "\\.git")))
 '(ido-minibuffer-setup-hook (quote ((lambda nil (local-set-key "" (quote ido-prev-match))) (lambda nil (local-set-key "" (quote ido-next-match))))))
 '(ido-mode (quote both) nil (ido))
 '(ido-rotate-file-list-default t)
 '(ido-save-directory-list-file "~/.emacs.d/tmp/.ido.last")
 '(image-dired-cmd-create-temp-image-options "%p -size %wx%h \"%f\" -resize \"%wx%h>\" jpeg:\"%t\"")
 '(image-dired-cmd-create-thumbnail-options "%p -size %wx%h \"%f\" -resize \"%wx%h>\" jpeg:\"%t\"")
 '(image-dired-dir "~/.emacs.d/tmp/image-dired/")
 '(indent-tabs-mode nil)
 '(inferior-ess-exit-command "q(save=\"no\")
")
 '(inhibit-startup-screen t)
 '(ispell-program-name "/usr/bin/hunspell")
 '(js-indent-level 2)
 '(keyfreq-autosave-mode t)
 '(keyfreq-file "~/.emacs.d/tmp/keyfreq")
 '(keyfreq-file-lock "~/.emacs.d/tmp/keyfreq.lock")
 '(keyfreq-mode t)
 '(line-number-mode t)
 '(linum-delay t)
 '(load-dir-ignore-errors t)
 '(load-dirs t)
 '(load-home-init-file t t)
 '(local-conf-helm-descbinds-active t nil (helm-descbinds))
 '(ls-lisp-dirs-first t)
 '(magit-diff-refine-hunk (quote all))
 '(magit-process-connection-type nil)
 '(menu-bar-mode nil)
 '(minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(mumamo-noweb2-mode-from-ext (quote (("php" . php+-mode) ("c" . c-mode))))
 '(nxhtml-skip-welcome t)
 '(nxml-mode-hook (quote (nxml-enable-unicode-char-name-sets rng-nxml-mode-init turn-on-flyspell)))
 '(nxml-slash-auto-complete-flag t)
 '(occur-mode-hook (quote (turn-on-font-lock linum-off turn-off-flyspell)))
 '(org-agenda-files (quote ("~/org/root.org")))
 '(org-archive-location "::* Archived")
 '(org-babel-load-languages (quote ((js . t) (dot . t) (latex . t) (ruby . t) (R . t) (python . t) (emacs-lisp . t) (sh . t) (sql . t))))
 '(org-babel-no-eval-on-ctrl-c-ctrl-c t)
 '(org-capture-templates (quote (("t" "Todo" entry (file+headline "~/org/root.org" "Task Inbox") "** TODO %?
   %i") ("d" "Day Page" entry (function org-open-day-page) "
* Sleep" :immediate-finish t :unnarrowed t))))
 '(org-clock-in-resume t)
 '(org-clock-into-drawer t)
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-persist (quote clock))
 '(org-clock-persist-file "~/.emacs.d/tmp/org-clock-save.el")
 '(org-default-notes-file "~/org/root.org")
 '(org-e-latex-minted-langs (quote ((emacs-lisp "common-lisp") (cc "c++") (cperl "perl") (shell-script "bash") (caml "ocaml") (ruby "ruby") (sql "sql"))))
 '(org-e-latex-pdf-process (quote ("latexmk --quiet %f")))
 '(org-export-latex-classes (quote (("article" "\\documentclass[11pt]{article}" ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}") ("\\subparagraph{%s}" . "\\subparagraph*{%s}")) ("report" "\\documentclass[11pt]{report}" ("\\part{%s}" . "\\part*{%s}") ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) ("book" "\\documentclass[11pt]{book}" ("\\part{%s}" . "\\part*{%s}") ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) ("beamer" "\\documentclass{beamer}" org-beamer-sectioning) ("vita" "\\documentclass{vita}" ("\\section{%s \\hrulefill}" . "\\section*{%s \\hrulefill}") ("\\subsection{%s}" . "\\subsection*{%s}")))))
 '(org-export-latex-default-packages-alist (quote (("" "xunicode" t) ("" "fontspec" t) ("" "xltxtra" t) ("" "fixltx2e" nil) ("" "graphicx" t) ("" "longtable" nil) ("" "float" nil) ("" "wrapfig" nil) ("" "soul" t) ("" "marvosym" t) ("" "wasysym" t) ("" "latexsym" t) ("" "amssymb" t) ("bookmarks, colorlinks, breaklinks, pdfusetitle" "hyperref" t) "\\tolerance=1000")))
 '(org-export-latex-listings (quote minted))
 '(org-export-latex-minted-langs (quote ((emacs-lisp "common-lisp") (cc "c++") (cperl "perl") (shell-script "bash") (caml "ocaml") (javascript "javascript") (sql "sql") (ruby "ruby"))))
 '(org-export-latex-packages-alist (quote (("" "minted" t))))
 '(org-id-locations-file "~/.emacs.d/tmp/.org-id-locations")
 '(org-latex-to-pdf-process (quote ("latexmk -f --quiet %f")))
 '(org-log-done (quote time))
 '(org-mode-hook (quote (flyspell-mode)))
 '(org-modules (quote (org-bibtex org-info org-jsinfo org-habit org-irc org-protocol org-special-blocks)))
 '(org-registry-file "~/.emacs.d/tmp/org-registry.el")
 '(org-return-follows-link t)
 '(org-todo-keywords (quote ((sequence "TODO" "STARTED" "WAITING" "DONE"))))
 '(org-track-directory "~/.emacs.d/elisp/")
 '(org-track-remove-package t)
 '(package-archive-upload-base "~/Dropbox/Public/elpa-haxney")
 '(package-archives (quote (("elpa-haxney" . "http://dl.dropbox.com/u/19422084/elpa-haxney/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/") ("elpa" . "http://tromey.com/elpa/") ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(package-base "~/Projects/elpa/")
 '(persp-completing-func (quote ido-completing-read))
 '(pgg-cache-passphrase nil)
 '(pgg-default-user-id "A016D1D6")
 '(php-basic-offset 2)
 '(php-mode-hook (quote (c-style-drupal)) t)
 '(pop-up-windows nil)
 '(popcmp-group-alternatives nil)
 '(predictive-auto-complete nil)
 '(predictive-auto-learn t)
 '(predictive-dict-autosave-on-kill-buffer nil)
 '(predictive-dict-autosave-on-mode-disable nil)
 '(predictive-main-dict (quote (my-dict dict-english)))
 '(psl-program-name "psl-interp")
 '(quack-dir "~/.emacs.d/tmp/quack")
 '(quack-global-menu-p nil)
 '(quack-pltish-keywords-to-fontify (quote ("and" "begin" "begin0" "c-declare" "c-lambda" "case" "case-lambda" "class" "class*" "class*/names" "class100" "class100*" "compound-unit/sig" "cond" "cond-expand" "define" "define-class" "define-compound-unit" "define-const-structure" "define-constant" "define-embedded" "define-entry-point" "define-external" "define-for-syntax" "define-foreign-record" "define-foreign-type" "define-foreign-variable" "define-generic" "define-generic-procedure" "define-inline" "define-location" "define-macro" "define-method" "define-module" "define-opt" "define-public" "define-reader-ctor" "define-record" "define-record-printer" "define-record-type" "define-signature" "define-struct" "define-structure" "define-syntax" "define-syntax-set" "define-values" "define-values-for-syntax" "define-values/invoke-unit/infer" "define-values/invoke-unit/sig" "define/contract" "define/override" "define/private" "define/public" "define/kw" "delay" "do" "else" "exit-handler" "field" "if" "import" "inherit" "inherit-field" "init" "init-field" "init-rest" "instantiate" "interface" "lambda" "lambda/kw" "let" "let*" "let*-values" "let+" "let-syntax" "let-values" "let/ec" "letrec" "letrec-values" "letrec-syntax" "match-lambda" "match-lambda*" "match-let" "match-let*" "match-letrec" "match-define" "mixin" "module" "opt-lambda" "or" "override" "override*" "namespace-variable-bind/invoke-unit/sig" "parameterize" "parameterize*" "parameterize-break" "private" "private*" "protect" "provide" "provide-signature-elements" "provide/contract" "public" "public*" "quasiquote" "quasisyntax" "quasisyntax/loc" "quote" "receive" "rename" "require" "require-for-syntax" "send" "send*" "set!" "set!-values" "signature->symbols" "super-instantiate" "syntax" "syntax/loc" "syntax-case" "syntax-case*" "syntax-error" "syntax-rules" "unit/sig" "unless" "unquote" "unquote-splicing" "when" "with-handlers" "with-method" "with-syntax" "define-type-alias" "define-type" "define-struct:" "define:" "let:" "letrec:" "let*:" "lambda:" "plambda:" "case-lambda:" "pcase-lambda:" "require/typed" "require/opaque-type" "require-typed-struct" "inst" "ann")))
 '(quack-pretty-lambda-p nil)
 '(quack-run-scheme-always-prompts-p nil)
 '(quack-smart-open-paren-p nil)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/tmp/recentf")
 '(require-final-newline t)
 '(safe-local-variable-values (quote ((require-final-newline . t) (buffer-file-coding-system . utf-8-unix) (whitespace-style face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark) (outline-minor-mode) (ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby") (lexical-binding . t))))
 '(save-completions-file-name "~/.emacs.d/tmp/completions")
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/tmp/places")
 '(save-place-limit 1000)
 '(savehist-file "~/.emacs.d/tmp/history")
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-error-top-bottom t)
 '(scss-compile-at-save nil)
 '(select-active-regions nil)
 '(semantic-default-submodes (quote (global-semantic-highlight-func-mode global-semantic-decoration-mode global-semantic-stickyfunc-mode global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-summary-mode global-semantic-mru-bookmark-mode)))
 '(semantic-mode t)
 '(semantic-python-dependency-system-include-path (quote ("/usr/local/lib/python2.6/dist-packages/Couchapp-0.3.32-py2.6.egg" "/usr/local/lib/python2.6/dist-packages/paisley-0.1-py2.6.egg" "/usr/local/lib/python2.6/dist-packages/feedparser-4.1-py2.6.egg" "/usr/local/lib/python2.6/dist-packages/python_twitter-0.6-py2.6.egg" "/usr/local/lib/python2.6/dist-packages/twython-0.9-py2.6.egg" "/usr/local/lib/python2.6/dist-packages/tweepy-1.3-py2.6.egg" "/usr/lib/python2.6" "/usr/lib/python2.6/plat-linux2" "/usr/lib/python2.6/lib-tk" "/usr/lib/python2.6/lib-old" "/usr/lib/python2.6/lib-dynload" "/usr/lib/python2.6/dist-packages" "/usr/lib/python2.6/dist-packages/PIL" "/usr/lib/python2.6/dist-packages/gst-0.10" "/usr/lib/pymodules/python2.6" "/usr/lib/python2.6/dist-packages/gtk-2.0" "/usr/lib/pymodules/python2.6/gtk-2.0" "/usr/lib/python2.6/dist-packages/wx-2.8-gtk2-unicode" "/usr/local/lib/python2.6/dist-packages" "/usr/lib/pymodules/python2.6/IPython/Extensions")))
 '(semanticdb-default-save-directory "~/.emacs.d/tmp/semanticdb")
 '(send-mail-function (quote mailclient-send-it))
 '(sentence-end-double-space nil)
 '(server-auth-dir "~/.emacs.d/tmp/server/")
 '(server-visit-hook (quote (server-edit-presets)))
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(smart-tab-using-hippie-expand t)
 '(smex-save-file "~/.emacs.d/tmp/smex.save")
 '(smiley-data-directory "~/.emacs.d/smileys")
 '(split-width-threshold nil)
 '(srecode-map-save-file "~/.emacs.d/tmp/srecode-map")
 '(text-mode-hook (quote (turn-on-flyspell turn-on-auto-fill)))
 '(tidy-temp-directory "/tmp/")
 '(tool-bar-mode nil)
 '(tramp-completion-reread-directory-timeout 5)
 '(tramp-default-method "ssh")
 '(tramp-gvfs-methods (quote ("dav" "davs" "obex" "synce" "smb" "ftp" "sftp")))
 '(tramp-persistency-file-name "~/.emacs.d/tmp/tramp")
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(undo-tree-mode-lighter "")
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(url-automatic-caching t)
 '(url-cache-directory "~/.emacs.d/tmp/url-cache")
 '(url-cookie-file "~/.emacs.d/tmp/url/cookies")
 '(vc-display-status nil)
 '(vc-handled-backends nil)
 '(view-mode-hook (quote (less-minor-mode scroll-lock-mode)))
 '(wdired-use-dired-vertical-movement (quote sometimes))
 '(weblogger-config-alist (quote (("haxney.org" "http://www.blogger.com/api" "dan@haxney.org" "" "7770370347473031286"))))
 '(windmove-wrap-around t)
 '(woman-cache-filename "~/.emacs.d/tmp/woman-cache.el")
 '(woman-imenu t)
 '(woman-post-format-hook (quote (less-minor-mode scroll-lock-mode)))
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
 '(helm-selection ((t (:inherit highlight))))
 '(helm-source-header ((t (:background "pale goldenrod" :underline t :weight semi-bold))))
 '(highline-face ((t (:background "pale turquoise"))) t)
 '(hl-line ((t (:background "pale turquoise"))))
 '(magit-item-highlight ((t nil)))
 '(whizzy-point-face ((((class color)) nil)) t))
