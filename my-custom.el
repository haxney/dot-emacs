(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -shell-escape")
 '(bm-repository-file "/home/dhackney/.emacs.d/bm-repository")
 '(bongo-enabled-backends (quote (mplayer)))
 '(bongo-mode-line-indicator-format (quote ((bongo-mode-line-pad-string) (when (bongo-hyphen-padded-mode-line-p) "[") (bongo-mode-line-previous-button) (bongo-mode-line-pause/resume-button) (bongo-mode-line-start/stop-button) (bongo-mode-line-next-button) (when (bongo-playing-p) " ") (when (bongo-playing-p) (cond ((and (bongo-elapsed-time) (bongo-total-time)) (format "%d%%" (/ (* 100.0 (bongo-elapsed-time)) (bongo-total-time)))) ((bongo-elapsed-time) (bongo-format-seconds (bongo-elapsed-time))))) " " (bongo-format-infoset (bongo-player-infoset (with-bongo-playlist-buffer bongo-player))) (when (bongo-hyphen-padded-mode-line-p) "]") (when (bongo-hyphen-padded-mode-line-p) (bongo-mode-line-pad-string)))))
 '(case-fold-search t)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(ecb-layout-name "left8")
 '(ecb-layout-window-sizes (quote (("left8" (0.22137404580152673 . 0.28125) (0.22137404580152673 . 0.25) (0.22137404580152673 . 0.265625) (0.22137404580152673 . 0.1875)))))
 '(ecb-options-version "2.32")
 '(ecb-tip-of-the-day nil)
 '(fill-column 80)
 '(git-append-signed-off-by t)
 '(gnus-init-file "~/.emacs.d/gnus/init.el")
 '(gnus-startup-file "~/.emacs.d/gnus/newsrc")
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "_whizzy_.*" ".*\\.raux" ".*\\.wdvi" "\\.git")))
 '(ido-minibuffer-setup-hook (quote ((lambda nil (local-set-key "" (quote ido-prev-match))) (lambda nil (local-set-key "" (quote ido-next-match))))))
 '(ido-save-directory-list-file "~/.emacs.d/.ido.last" t)
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
 '(menu-bar-mode nil)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files (quote ("~/org/root.org")))
 '(org-archive-location "%s_archive::* Archived Tasks")
 '(org-default-notes-file "~/org/root.org")
 '(org-remember-templates (quote (("" 116 "** TODO %?
  %i
 %a" "~/org/root.org" "") ("" 97 "** Appointment: %?
%^T
%i
  %a" "~/org/root.org" ""))))
 '(org-todo-keywords (quote ((sequence "TODO" "STARTED" "WAITING" "DONE"))))
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
 '(show-paren-mode t nil (paren))
 '(tab-width 4)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(woman-use-own-frame nil)
 '(xgit-use-index (quote always)))
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
 '(flyspell-incorrect-face ((((class color)) (:foreground "magenta" :underline t :weight bold))) t))
