(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -shell-escape")
 '(LaTeX-mode-hook (quote (flymake-mode-off flyspell-mode)) t)
 '(TeX-master nil)
 '(after-save-hook (quote (executable-make-buffer-file-executable-if-script-p)))
 '(auto-save-list-file-prefix "~/.emacs.d/tmp/auto-save-list/.saves-")
 '(backup-directory-alist (quote (("." . "~/.emacs.d/tmp/baks/"))))
 '(bcc-blacklist (quote ("/\\.recentf$" "/history$" "auctex" "msf-abbrev")))
 '(bcc-cache-directory "~/.emacs.d/tmp/byte-cache")
 '(blink-cursor-mode nil)
 '(bm-repository-file "~/.emacs.d/tmp/bm-repository")
 '(bookmark-default-file "~/.emacs.d/tmp/bookmark")
 '(case-fold-search t)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(current-language-environment "UTF-8")
 '(default-frame-alist (quote ((cursor-type bar . 2) (font . "Monospace-8"))))
 '(default-input-method "rfc1345")
 '(default-major-mode (quote text-mode))
 '(ecb-layout-name "left8")
 '(ecb-layout-window-sizes (quote (("left8" (0.22137404580152673 . 0.28125) (0.22137404580152673 . 0.25) (0.22137404580152673 . 0.265625) (0.22137404580152673 . 0.1875)))))
 '(ecb-options-version "2.32")
 '(ecb-tip-of-the-day nil)
 '(ede-project-placeholder-cache-file "~/.emacs.d/tmp/projects.ede")
 '(fill-column 80)
 '(flymake-allowed-file-name-masks (quote (("\\.c\\'" flymake-simple-make-init) ("\\.cpp\\'" flymake-simple-make-init) ("\\.xml\\'" flymake-xml-init) ("\\.html?\\'" flymake-xml-init) ("\\.cs\\'" flymake-simple-make-init) ("\\.p[ml]\\'" flymake-perl-init) ("\\.php[345]?\\'" flymake-php-init) ("\\.module\\'" flymake-php-init) ("\\.inc\\'" flymake-php-init) ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup) ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup) ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup) ("\\.tex\\'" flymake-simple-tex-init) ("\\.idl\\'" flymake-simple-make-init))))
 '(flyspell-use-meta-tab nil)
 '(git-append-signed-off-by t)
 '(global-linum-mode t)
 '(hippie-expand-dabbrev-as-symbol nil)
 '(ido-everywhere t)
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "_whizzy_.*" ".*\\.raux" ".*\\.wdvi" "\\.git")))
 '(ido-minibuffer-setup-hook (quote ((lambda nil (local-set-key "" (quote ido-prev-match))) (lambda nil (local-set-key "" (quote ido-next-match))))))
 '(ido-save-directory-list-file "~/.emacs.d/tmp/.ido.last")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
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
 '(nxml-mode-hook (quote (nxml-enable-unicode-char-name-sets rng-nxml-mode-init flyspell-mode)))
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
 '(persp-completing-func (quote ido-completing-read))
 '(pgg-cache-passphrase nil)
 '(pgg-default-user-id "A016D1D6")
 '(planner-reverse-chronological-notes nil)
 '(quack-default-program "mzscheme -g")
 '(quack-global-menu-p nil)
 '(quack-pretty-lambda-p nil)
 '(quack-run-scheme-always-prompts-p nil)
 '(quack-smart-open-paren-p nil)
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-data-file "~/org/root.org")
 '(remember-handler-functions (quote (org-remember-handler)))
 '(remember-mode-hook (quote (flyspell-mode turn-on-auto-fill org-remember-apply-template)))
 '(require-final-newline t)
 '(server-switch-hook (quote (flyspell-mode)))
 '(show-paren-mode t nil (paren))
 '(smex-save-file "~/.emacs.d/tmp/smex.save")
 '(tab-width 4)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(tramp-completion-reread-directory-timeout 5)
 '(tramp-default-method "ssh")
 '(tramp-persistency-file-name "~/.emacs.d/tmp/tramp")
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(windmove-wrap-around t)
 '(woman-use-own-frame nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed-face :background "Green"))))
 '(diff-added-face ((t (:inherit diff-changed-face :background "Green"))) t)
 '(diff-removed ((t (:inherit diff-changed-face :background "red"))))
 '(diff-removed-face ((t (:inherit diff-changed-face :background "red"))) t)
 '(flyspell-duplicate-face ((((class color)) (:foreground "Gold3" :underline t :weight bold))) t)
 '(flyspell-incorrect-face ((((class color)) (:foreground "magenta" :underline t :weight bold))) t)
 '(whizzy-point-face ((((class color)) nil))))
