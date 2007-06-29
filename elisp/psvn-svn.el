;;; psvn-svn.el --- Subversion support for psvn.el / Emacs
;; Copyright (C) 2002-2006 by Stefan Reichoer
;; $Id$
;; $URL$

(defun svn-svn-registered (file)
  "Return true if FILE is registered under Subversion."
  ;; a quick false positive test: is there a `.svn/entries' file?
  (file-exists-p (expand-file-name (concat (svn-wc-adm-dir-name) "/entries")
                                   (file-name-directory (concat file "/")))))

;;;###autoload
(defun svn-svn-status (dir &optional arg)
  "Implementation of `svn-status' for the SVN backend."
  (unless (file-directory-p dir)
    (error "%s is not a directory" dir))
  (if (not (svn-svn-registered dir))
      (when (y-or-n-p
             (concat dir
                     " does not seem to be a Subversion working copy (no "
                     (svn-wc-adm-dir-name) " directory).  "
                     "Run dired instead? "))
        (dired dir))
    (setq dir (file-name-as-directory dir))
    (when svn-status-load-state-before-svn-status
      (unless (string= dir (car svn-status-directory-history))
        (svn-status-load-state t)))
    (setq svn-status-directory-history (delete dir svn-status-directory-history))
    (add-to-list 'svn-status-directory-history dir)
    (if (string= (buffer-name) svn-status-buffer-name)
        (setq svn-status-display-new-status-buffer nil)
      (setq svn-status-display-new-status-buffer t)
      ;;(message "psvn: Saving initial window configuration")
      (setq svn-status-initial-window-configuration (current-window-configuration)))
    (let* ((cur-buf (current-buffer))
           (status-buf (get-buffer-create svn-status-buffer-name))
           (proc-buf (get-buffer-create svn-process-buffer-name))
           (want-edit (eq arg '-))
           (status-option (if want-edit
                            (if svn-status-verbose "-v" "")
                          (if svn-status-verbose
                              (if arg "-uv" "-v")
                            (if arg "-u" ""))))
         (svn-status-edit-svn-command
          (or want-edit svn-status-edit-svn-command)))
      (save-excursion
        (set-buffer status-buf)
        (setq default-directory dir)
        (set-buffer proc-buf)
        (setq default-directory dir
              svn-status-remote (when arg t))
        (set-buffer cur-buf)
        (svn-svn-run t t 'status "status" status-option)))))

(defun svn-svn-run (run-asynchron clear-process-buffer cmdtype &rest arglist)
  "Implementation of `svn-run' for the SVN backend."
  (setq arglist (svn-status-flatten-list arglist))
  (if (eq (process-status "svn") nil)
      (progn
        (when svn-status-edit-svn-command
          (setq arglist (append
                         (list (car arglist))
                         (split-string
                          (read-from-minibuffer
                           (format "svn %s flags: " (car arglist))
                           (mapconcat 'identity (cdr arglist) " ")))))
          (when (eq svn-status-edit-svn-command t)
            (svn-status-toggle-edit-cmd-flag t))
          (message "svn-svn-run %s: %S" cmdtype arglist))
        (run-hooks 'svn-pre-run-hook)
        (unless (eq mode-line-process 'svn-status-mode-line-process)
          (setq svn-pre-run-mode-line-process mode-line-process)
          (setq mode-line-process 'svn-status-mode-line-process))
        (setq svn-status-pre-run-svn-buffer (current-buffer))
        (let* ((proc-buf (get-buffer-create svn-process-buffer-name))
               (svn-exe svn-status-svn-executable)
               (svn-proc))
          (when (listp (car arglist))
            (setq arglist (car arglist)))
          (save-excursion
            (set-buffer proc-buf)
            (setq buffer-read-only nil)
            (buffer-disable-undo)
            (fundamental-mode)
            (if clear-process-buffer
                (delete-region (point-min) (point-max))
              (goto-char (point-max)))
            (setq svn-process-cmd cmdtype)
            (setq svn-status-last-commit-author nil)
            (setq svn-status-mode-line-process-status (format " running %s" cmdtype))
            (svn-status-update-mode-line)
            (sit-for 0.1)
            (ring-insert svn-last-cmd-ring (list (current-time-string) arglist default-directory))
            (if run-asynchron
                (progn
                  ;;(message "running asynchron: %s %S" svn-exe arglist)
                  (setq svn-pre-run-asynch-recent-keys (recent-keys))
                  (let ((process-environment (svn-process-environment))
                        (process-connection-type nil))
                    ;; Communicate with the subprocess via pipes rather
                    ;; than via a pseudoterminal, so that if the svn+ssh
                    ;; scheme is being used, SSH will not ask for a
                    ;; passphrase via stdio; psvn.el is currently unable
                    ;; to answer such prompts.  Instead, SSH will run
                    ;; x11-ssh-askpass if possible.  If Emacs is being
                    ;; run on a TTY without $DISPLAY, this will fail; in
                    ;; such cases, the user should start ssh-agent and
                    ;; then run ssh-add explicitly.
                    (setq svn-proc (apply 'start-process "svn" proc-buf svn-exe arglist)))
                  (when svn-status-svn-process-coding-system
                    (set-process-coding-system svn-proc svn-status-svn-process-coding-system
                                               svn-status-svn-process-coding-system))
                  (set-process-sentinel svn-proc 'svn-process-sentinel)
                  (when svn-status-track-user-input
                    (set-process-filter svn-proc 'svn-process-filter)))
              ;;(message "running synchron: %s %S" svn-exe arglist)
              (let ((process-environment (svn-process-environment)))
                ;; `call-process' ignores `process-connection-type' and
                ;; never opens a pseudoterminal.
                (apply 'call-process svn-exe nil proc-buf nil arglist))
              (setq svn-status-last-output-buffer-name svn-process-buffer-name)
              (run-hooks 'svn-post-process-svn-output-hook)
              (setq svn-status-mode-line-process-status "")
              (svn-status-update-mode-line)
              (when svn-pre-run-mode-line-process
                (setq mode-line-process svn-pre-run-mode-line-process)
                (setq svn-pre-run-mode-line-process nil))))))
    (error "You can only run one svn process at once!")))

(defun svn-svn-status-parse-commit-output ()
  "Implementation of `svn-status-parse-commit-output' for the SVN backend."
  (save-excursion
    (set-buffer svn-process-buffer-name)
    (let ((action)
          (file-name)
          (skip)
          (result))
      (goto-char (point-min))
      (setq svn-status-commit-rev-number nil)
      (setq skip nil) ; set to t whenever we find a line not about a committed file
      (while (< (point) (point-max))
        (cond ((= (svn-point-at-eol) (svn-point-at-bol)) ;skip blank lines
               (setq skip t))
              ((looking-at "Sending")
               (setq action 'committed))
              ((looking-at "Adding")
               (setq action 'added))
              ((looking-at "Deleting")
               (setq action 'deleted))
              ((looking-at "Replacing")
               (setq action 'replaced))
              ((looking-at "Transmitting file data")
               (setq skip t))
              ((looking-at "Committed revision \\([0-9]+\\)")
               (setq svn-status-commit-rev-number
                     (string-to-number (svn-match-string-no-properties 1)))
               (setq skip t))
              (t ;; this should never be needed(?)
               (setq action 'unknown)))
        (unless skip                                ;found an interesting line
          (forward-char 15)
          (when svn-status-operated-on-dot
            ;; when the commit used . as argument, delete the trailing directory
            ;; from the svn output
            (search-forward "/" nil t))
          (setq file-name (buffer-substring-no-properties (point) (svn-point-at-eol)))
          (unless svn-status-last-commit-author
            (setq svn-status-last-commit-author (car (svn-status-info-for-path (expand-file-name (concat default-directory file-name))))))
          (setq result (cons (list file-name action)
                             result))
          (setq skip nil))
        (forward-line 1))
      result)))
;;(svn-status-parse-commit-output)
;;(svn-status-annotate-status-buffer-entry)

(defun svn-svn-status-parse-ar-output ()
  "Implementation of `svn-status-parse-ar-output' for the SVN backend."
  (save-excursion
    (set-buffer svn-process-buffer-name)
    (let ((action)
          (name)
          (skip)
          (result))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (cond ((= (svn-point-at-eol) (svn-point-at-bol)) ;skip blank lines
               (setq skip t))
              ((looking-at "A")
               (setq action 'added-wc))
              ((looking-at "D")
               (setq action 'deleted-wc))
              (t ;; this should never be needed(?)
               (setq action 'unknown)))
        (unless skip ;found an interesting line
          (forward-char 10)
          (setq name (buffer-substring-no-properties (point) (svn-point-at-eol)))
          (setq result (cons (list name action)
                             result))
          (setq skip nil))
        (forward-line 1))
      result)))
;; (svn-status-parse-ar-output)
;; (svn-status-update-with-command-list (svn-status-parse-ar-output))

(defun svn-svn-status-parse-update-output ()
  "Implementation of `svn-status-parse-update-output' for the SVN backend."
  (save-excursion
    (set-buffer svn-process-buffer-name)
    (setq svn-status-update-rev-number nil)
    (let ((action)
          (name)
          (skip)
          (result))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (cond ((= (svn-point-at-eol) (svn-point-at-bol)) ;skip blank lines
               (setq skip t))
              ((looking-at "Updated to revision \\([0-9]+\\)")
               (setq svn-status-update-rev-number
                     (list t (string-to-number (svn-match-string-no-properties 1))))
               (setq skip t))
              ((looking-at "At revision \\([0-9]+\\)")
               (setq svn-status-update-rev-number
                     (list nil (string-to-number (svn-match-string-no-properties 1))))
               (setq skip t))
              ((looking-at "U")
               (setq action 'updated))
              ((looking-at "A")
               (setq action 'added))
              ((looking-at "D")
               (setq skip t))
               ;;(setq action 'deleted)) ;;deleted files are not displayed in the svn status output.
              ((looking-at "C")
               (setq action 'conflicted))
              ((looking-at "G")
               (setq action 'merged))

              ((looking-at " U")
               (setq action 'updated-props))

              (t ;; this should never be needed(?)
               (setq action (concat "parse-update: '"
                                    (buffer-substring-no-properties (point) (+ 2 (point))) "'"))))
        (unless skip ;found an interesting line
          (forward-char 3)
          (setq name (buffer-substring-no-properties (point) (svn-point-at-eol)))
          (setq result (cons (list name action)
                             result))
          (setq skip nil))
        (forward-line 1))
      result)))
;; (svn-status-parse-update-output)
;; (svn-status-update-with-command-list (svn-status-parse-update-output))

(defun svn-svn-status-parse-property-output ()
  "Implementation of `svn-status-parse-property-output' for the SVN backend."
  (save-excursion
    (set-buffer svn-process-buffer-name)
    (let ((result))
      (dolist (line (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
        (message "%s" line)
        (when (string-match "property '\\(.+\\)' set on '\\(.+\\)'" line)
          ;;(message "property %s - file %s" (match-string 1 line) (match-string 2 line))
          (setq result (cons (list (match-string 2 line) 'propset) result))))
      result)))

(defun svn-svn-status-parse-info-result ()
  "Implementation of `svn-status-parse-info-result' for the SVN backend."
  (let ((url)
        (repository-root)
        (last-changed-author))
    (save-excursion
      (set-buffer svn-process-buffer-name)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (search-forward "url: ")
        (setq url (buffer-substring-no-properties (point) (svn-point-at-eol)))
        (when (search-forward "repository root: " nil t)
          (setq repository-root (buffer-substring-no-properties (point) (svn-point-at-eol))))
        (when (search-forward "last changed author: " nil t)
          (setq last-changed-author (buffer-substring-no-properties (point) (svn-point-at-eol))))))
    (setq svn-status-base-info `((url ,url) (repository-root ,repository-root) (last-changed-author ,last-changed-author)))))

(defun svn-svn-status-show-svn-log (arg)
  "Implementation of `svn-status-show-svn-log' for the SVN backend."
  (let ((switches (cond ((eq arg 0)  '("-q"))
                        ((eq arg -1) '())
                        (arg         '("-v"))
                        (t           svn-status-default-log-arguments))))
    (svn-status-create-arg-file svn-status-temp-arg-file "" (svn-status-marked-files) "")
    (svn-svn-run t t 'log "log" "--targets" svn-status-temp-arg-file switches)
    (save-excursion
      (set-buffer svn-process-buffer-name)
      (svn-log-view-mode))))

(defun svn-svn-status-version ()
  "Show the version numbers for psvn.el and the svn command line client.
The version number of the client is cached in `svn-client-version'."
  (interactive)
  (let ((window-conf (current-window-configuration))
        (version-string))
    (if (or (interactive-p) (not svn-status-cached-version-string))
        (progn
          (svn-svn-run nil t 'version "--version")
          (when (interactive-p)
            (svn-status-show-process-output 'info t))
          (with-current-buffer svn-status-last-output-buffer-name
            (goto-char (point-min))
            (setq svn-client-version
                  (when (re-search-forward "svn, version \\([0-9\.]+\\) " nil t)
                    (mapcar 'string-to-number (split-string (match-string 1) "\\."))))
            (let ((buffer-read-only nil))
              (goto-char (point-min))
              (insert (format "psvn.el revision: %s\n\n" svn-psvn-revision)))
            (setq version-string (buffer-substring-no-properties (point-min) (point-max))))
          (setq svn-status-cached-version-string version-string))
      (setq version-string svn-status-cached-version-string)
    (unless (interactive-p)
      (set-window-configuration window-conf)
      version-string))))

(defun svn-svn-status-info ()
  "Run `svn info' on all selected files.
See `svn-status-marked-files' for what counts as selected."
  (interactive)
  (svn-status-create-arg-file svn-status-temp-arg-file "" (svn-status-marked-files) "")
  (svn-run t t 'info "info" "--targets" svn-status-temp-arg-file))

(defun svn-svn-status-export (src dst)
  "Implementation of `svn-status-export' for the SVN backend."
  (svn-svn-run t t 'export "export" src dst))

(defun svn-svn-status-get-specific-revision-internal (line-infos revision)
  "Implementation of `svn-status-get-specific-revision-internal' for the SVN backend."
  ;; In `svn-status-show-svn-diff-internal', there is a comment
  ;; that REVISION `nil' might mean omitting the -r option entirely.
  ;; That doesn't seem like a good idea with svn cat.

  ;; (message "svn-status-get-specific-revision-internal: %S %S" line-infos revision)

  (when (eq revision :ask)
    (setq revision (svn-status-read-revision-string
                    "Get files for version: " "PREV")))

  (let ((count (length line-infos)))
    (if (= count 1)
        (let ((line-info (car line-infos)))
          (message "Getting revision %s of %s"
                   (if (eq revision :auto)
                       (if (svn-status-line-info->update-available line-info)
                           "HEAD" "BASE")
                     revision)
                   (svn-status-line-info->filename line-info)))
      ;; We could compute "Getting HEAD of 8 files and BASE of 11 files"
      ;; but that'd be more bloat than it's worth.
      (message "Getting revision %s of %d files"
               (if (eq revision :auto) "HEAD or BASE" revision)
               count)))

  (let ((svn-status-get-specific-revision-file-info '()))
    (dolist (line-info line-infos)
      (let* ((revision (if (eq revision :auto)
                           (if (svn-status-line-info->update-available line-info)
                               "HEAD" "BASE")
                         revision))    ;must be a string by this point
             (file-name (svn-status-line-info->filename line-info))
             ;; If REVISION is e.g. "HEAD", should we find out the actual
             ;; revision number and save "foo.~123~" rather than "foo.~HEAD~"?
             ;; OTOH, `auto-mode-alist' already ignores ".~HEAD~" suffixes,
             ;; and if users often want to know the revision numbers of such
             ;; files, they can use svn:keywords.
             (file-name-with-revision (concat (file-name-nondirectory file-name) ".~" revision "~"))
             (default-directory (concat (svn-svn-status-base-dir) (file-name-directory file-name))))
        ;; `add-to-list' would unnecessarily check for duplicates.
        (push (cons file-name (concat (file-name-directory file-name) file-name-with-revision)) svn-status-get-specific-revision-file-info)
        ;; (message "file-name-with-revision: %s %S" file-name-with-revision (file-exists-p file-name-with-revision))
        (save-excursion
          (if (or (not (file-exists-p file-name-with-revision)) ;; file does not exist
                  (not (string= (number-to-string (string-to-number revision)) revision))) ;; revision is not a number
              (progn
                (message "getting revision %s for %s" revision file-name)
                (let ((content
                       (with-temp-buffer
                         (if (string= revision "BASE")
                             (insert-file-contents (concat (svn-wc-adm-dir-name)
                                                           "/text-base/"
                                                           (file-name-nondirectory file-name)
                                                           ".svn-base"))
                           (progn
                             (svn-run nil t 'cat "cat" "-r" revision (file-name-nondirectory file-name))
                             ;;todo: error processing
                             ;;svn: Filesystem has no item
                             ;;svn: file not found: revision `15', path `/trunk/file.txt'
                             (insert-buffer-substring svn-process-buffer-name)))
                         (buffer-string))))
                  (find-file file-name-with-revision)
                  (setq buffer-read-only nil)
                  (erase-buffer) ;Widen, because we'll save the whole buffer.
                  (insert content)
                  (goto-char (point-min))
                  (save-buffer)))
            (find-file file-name-with-revision)))))
    ;;(message "default-directory: %s revision-file-info: %S" default-directory svn-status-get-specific-revision-file-info)
    (nreverse svn-status-get-specific-revision-file-info)))

(defun svn-svn-status-svnversion ()
  "Implementation of `svn-status-svnversion' for the SVN backend."
  (svn-status-ensure-cursor-on-file)
  (let ((simple-path (svn-status-line-info->filename (svn-status-get-line-information)))
        (full-path (svn-status-line-info->full-path (svn-status-get-line-information)))
        (version))
    (unless (file-directory-p simple-path)
      (setq simple-path (or (file-name-directory simple-path) "."))
      (setq full-path (file-name-directory full-path)))
    (setq version (shell-command-to-string (concat "svnversion -n " full-path)))
    (message "svnversion for '%s': %s" simple-path version)
    version))

;; --------------------------------------------------------------------------------
;; status persistent options
;; --------------------------------------------------------------------------------

(defun svn-svn-status-base-dir (&optional start-directory)
  "Implementation of `svn-status-base-dir' for the SVN backend."
  (let* ((start-dir (expand-file-name (or start-directory default-directory)))
         (base-dir (gethash start-dir svn-status-base-dir-cache 'not-found)))
    (message "svn-svn-status-base-dir: %S %S" start-dir base-dir)
    (if (not (eq base-dir 'not-found))
        base-dir
      (message "calculating base-dir for %s" start-dir)
      (unless svn-client-version
        (svn-status-version))
      (let* ((base-dir start-dir)
             (repository-root (svn-status-repo-for-path base-dir))
             (dot-svn-dir (concat base-dir (svn-wc-adm-dir-name)))
             (in-tree (and repository-root (file-exists-p dot-svn-dir)))
             (dir-below (expand-file-name base-dir)))
        ;; (message "repository-root: %s start-dir: %s" repository-root start-dir)
        (if (and (<= (car svn-client-version) 1) (< (cadr svn-client-version) 3))
            (setq base-dir (svn-svn-status-base-dir-for-ancient-svn-client start-dir)) ;; svn version < 1.3
          (while (when (and dir-below (file-exists-p dot-svn-dir))
                   (setq base-dir (file-name-directory dot-svn-dir))
                   (string-match "\\(.+/\\).+/" dir-below)
                   (setq dir-below
                         (and (string-match "\\(.*/\\)[^/]+/" dir-below)
                              (match-string 1 dir-below)))
                   ;; (message "base-dir: %s, dir-below: %s, dot-svn-dir: %s in-tree: %s" base-dir dir-below dot-svn-dir in-tree)
                   (when dir-below
                     (if (string= (svn-status-repo-for-path dir-below) repository-root)
                         (setq dot-svn-dir (concat dir-below (svn-wc-adm-dir-name)))
                       (setq dir-below nil)))))
          (setq base-dir (and in-tree base-dir)))
        (svn-puthash start-dir base-dir svn-status-base-dir-cache)
        (svn-status-message 7 "svn-svn-status-base-dir %s => %s" start-dir base-dir)
        base-dir))))

(provide 'psvn-svn)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; psvn-svn.el ends here
