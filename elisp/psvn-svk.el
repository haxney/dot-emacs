;;; psvn-svk.el --- SVK support for psvn.el / Emacs
;; Copyright (C) 2002-2006 by Stefan Reichoer
;; $Id$
;; $URL$

;; TODO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; * 'svn export' equivalent with 'svk co'
;; * 'svnversion' has no SVK equivalent; emulate it?
;; * svn-svk-status-base-dir: find the base checkout dir instead of cheating
;; * svn-svk-status-show-svn-log should work on selected files
;; * defcustom a few variables
;; * use great ideas from vc-svk-co-* functions
;; * add SVK functions that SVN does not support
;; * submit SVK bug report for it's management of filenames starting with '++'.


;;; init

(require 'time-date)

; better keep 'SVN first
(add-to-list 'svn-handled-backends 'SVK t)

;;; custom

(defcustom svn-status-svk-executable "svk"
  "*The name of the svk executable.
This can be either absolute or looked up on `exec-path'."
  ;; Don't use (file :must-match t).  It doesn't know about `exec-path'.
  :type 'file
  :group 'psvn)
(put 'svn-status-svk-executable 'risky-local-variable t)

;;; Compatibility with Emacs <22

(if (fboundp 'time-less-p)
    (defalias 'svn-svk-time-less-p 'time-less-p)
  (defun svn-svk-time-less-p (t1 t2)
    "Say whether time value T1 is less than time value T2."
    (with-decoded-time-value ((high1 low1 micro1 t1)
                              (high2 low2 micro2 t2))
      (or (< high1 high2)
          (and (= high1 high2)
               (or (< low1 low2)
                   (and (= low1 low2)
                        (< micro1 micro2))))))))

(defun svn-svk-emacs-assoc-default (key alist &optional test default)
  "Find object KEY in a pseudo-alist ALIST.
ALIST is a list of conses or objects.  Each element (or the element's car,
if it is a cons) is compared with KEY by evaluating (TEST (car elt) KEY).
If that is non-nil, the element matches;
then `assoc-default' returns the element's cdr, if it is a cons,
or DEFAULT if the element is not a cons.

If no element matches, the value is nil.
If TEST is omitted or nil, `equal' is used."
  (let (found (tail alist) value)
    (while (and tail (not found))
      (let ((elt (car tail)))
	(when (funcall (or test 'equal) (if (consp elt) (car elt) elt) key)
	  (setq found t value (if (consp elt) (cdr elt) default))))
      (setq tail (cdr tail)))
    value))

(if (fboundp 'assoc-string)
    (defalias 'svn-svk-assoc-string 'assoc-string)
  (defun svn-svk-assoc-string (key alist)
    (svn-svk-emacs-assoc-default key alist
                   (lambda (a b)
                     (and (stringp a) (stringp b) (string-equal a b))))))


;;; Functions needed by psvn interface

(defun svn-svk-registered (file)
  "Check if FILE is SVK registered."
  (let ((lfile (file-truename file)))   ; SVK stores truenames
    (svn-svk-co-path-p lfile)))

;;;###autoload
(defun svn-svk-status (dir &optional arg)
  "Implementation of `svn-status' for the SVK backend."
  (unless (file-directory-p dir)
    (error "%s is not a directory" dir))
  (if (not (svn-svk-registered dir))
      (when (y-or-n-p
             (concat dir " does not seem to be a SVK working copy. "
                     "Run dired instead? "))
        (dired dir))
    (setq dir (file-name-as-directory dir))
    (setq default-directory dir)
    (when svn-status-load-state-before-svn-status
      (unless (string= dir (car svn-status-directory-history))
        (svn-status-load-state t)))
    (setq svn-status-directory-history (delete dir svn-status-directory-history))
    (add-to-list 'svn-status-directory-history dir)
    (if (string= (buffer-name) svn-status-buffer-name)
        (setq svn-status-display-new-status-buffer nil)
      (setq svn-status-display-new-status-buffer t)
      (setq svn-status-initial-window-configuration (current-window-configuration)))
    (let* ((status-buf (get-buffer-create svn-status-buffer-name))
           (proc-buf (get-buffer-create svn-process-buffer-name))
           (status-option (if svn-status-verbose "-v" "")))
      (save-excursion
        (set-buffer status-buf)
        (setq default-directory dir)
        (set-buffer proc-buf)
        (setq default-directory dir
              svn-status-remote (when arg t))
        (svn-svk-run t t 'status "status" status-option)))))

(defun svn-svk-run (run-asynchron clear-process-buffer cmdtype &rest arglist)
  "Implementation of `svn-run' for the SVK backend."
  (setq arglist (svn-status-flatten-list arglist))
  (if (eq (process-status "svk") nil)
      (progn
        (when svn-status-edit-svn-command
          (setq arglist (append
                         (list (car arglist))
                         (split-string
                          (read-from-minibuffer
                           (format "svk %s flags: " (car arglist))
                           (mapconcat 'identity (cdr arglist) " ")))))
          (when (eq svn-status-edit-svn-command t)
            (svn-status-toggle-edit-cmd-flag t))
          (message "svn-svk-run %s: %S" cmdtype arglist))
        (run-hooks 'svn-pre-run-hook)
        (unless (eq mode-line-process 'svn-status-mode-line-process)
          (setq svn-pre-run-mode-line-process mode-line-process)
          (setq mode-line-process 'svn-status-mode-line-process))
        (setq svn-status-pre-run-svn-buffer (current-buffer))
        (let* ((proc-buf (get-buffer-create svn-process-buffer-name))
               (svn-exe svn-status-svk-executable)
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
                    (setq svn-proc (apply 'start-process "svk" proc-buf svn-exe arglist)))
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
    (error "You can only run one svk process at once!")))

(defun svn-svk-status-parse-ar-output ()
  "Implementation of `svn-status-parse-ar-output' for the SVK backend."
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
          (forward-char 4)
          (setq name (buffer-substring-no-properties (point) (svn-point-at-eol)))
          (setq result (cons (list name action)
                             result))
          (setq skip nil))
        (forward-line 1))
      result)))

(defun svn-svk-status-parse-info-result ()
  "Implementation of `svn-status-parse-info-result' for the SVK backend."
  (let ((url)
        (repository-root)
        (last-changed-author))
    (save-excursion
      (set-buffer svn-process-buffer-name)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (search-forward "Depot Path: ")
        (setq url (buffer-substring-no-properties (point) (svn-point-at-eol)))))
    (setq svn-status-base-info `((url ,url) ))))

(defun svn-svk-status-show-svn-log (arg)
  "Implementation of `svn-status-show-svn-log' for the SVK backend."
  (let ((switches (cond ((eq arg 0)  '("-q"))
                        ((eq arg -1) '())
                        (arg         '("-v"))
                        (t           svn-status-default-log-arguments))))
    (svn-svk-run t t 'log "log" switches)
    (save-excursion
      (set-buffer svn-process-buffer-name)
      (svn-log-view-mode))))

(defun svn-svk-status-version ()
  "Show the version numbers for psvn.el and the svk command line client.
The version number of the client is cached in `svn-client-version'."
  (interactive)
  (let ((window-conf (current-window-configuration))
        (version-string))
    (if (or (interactive-p) (not svn-status-cached-version-string))
        (progn
          (svn-svk-run nil t 'version "--version")
          (when (interactive-p)
            (svn-status-show-process-output 'info t))
          (with-current-buffer svn-status-last-output-buffer-name
            (goto-char (point-min))
            (setq svn-client-version
                  (when (re-search-forward "This is svk, version \\([0-9\.]+\\)." nil t)
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
;; (svn-svk-status-version)

(defun svn-svk-status-get-specific-revision-internal (line-infos revision)
  "Implementation of `svn-status-get-specific-revision-internal' for the SVN backend."
  ;; In `svn-status-show-svn-diff-internal', there is a comment
  ;; that REVISION `nil' might mean omitting the -r option entirely.
  ;; That doesn't seem like a good idea with svn cat.

  (message "svn-status-get-specific-revision-internal: %S %S" line-infos revision)

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
                         revision))       ;must be a string by this point
             (file-name (svn-status-line-info->filename line-info))
             ;; If REVISION is e.g. "HEAD", should we find out the actual
             ;; revision number and save "foo.~123~" rather than "foo.~HEAD~"?
             ;; OTOH, `auto-mode-alist' already ignores ".~HEAD~" suffixes,
             ;; and if users often want to know the revision numbers of such
             ;; files, they can use svn:keywords.
             (file-name-with-revision (concat (file-name-nondirectory file-name) ".~" revision "~")))
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
                         (progn
                           (svn-svk-run nil t 'cat "cat" "-r" revision (file-name-nondirectory file-name))
                           ;;todo: error processing
                           ;;svn: Filesystem has no item
                           ;;svn: file not found: revision `15', path `/trunk/file.txt'
                           (insert-buffer-substring svn-process-buffer-name))
                         (buffer-string))))
                  (find-file file-name-with-revision)
                  (setq buffer-read-only nil)
                  (erase-buffer)  ;Widen, because we'll save the whole buffer.
                  (insert content)
                  (goto-char (point-min))
                  (save-buffer)))
            (find-file file-name-with-revision)))))
    ;;(message "default-directory: %s revision-file-info: %S" default-directory svn-status-get-specific-revision-file-info)
    (nreverse svn-status-get-specific-revision-file-info)))

;;; Aux. functions that will often avoid slow calls to svk.

(defvar svn-svk-co-paths nil)
(defun svn-svk-co-paths ()
  (interactive)
  (let ((config "~/.svk/config")
        mtime)
    (when (file-readable-p config)
      (setq mtime (nth 5 (file-attributes "~/.svk/config")))
      (unless (and svn-svk-co-paths           ; has not it been loaded?
                   (svn-svk-time-less-p mtime ; is it unmodified since?
                                       (car (last svn-svk-co-paths))))
        ;; (re)load
        (setq svn-svk-co-paths (list mtime))
        (with-temp-buffer
          (insert-file-contents config)
          (when (search-forward "hash:" nil t) ; to start of co paths
            (while (re-search-forward               ; to next co path
                    "^ +\\(/.*\\): *\n.*depotpath: \\(/.+\\)$" nil t)
              (add-to-list 'svn-svk-co-paths
                           (list (svn-match-string-no-properties 1)
                                 (svn-match-string-no-properties 2)))))))))
  svn-svk-co-paths)

(defun svn-svk-co-path-p (file)
  "Whether SVK manages a parent directory of FILE.
Note that this does not try to guarantee SVK manages this particular
subdirectory. That's for the full `svn-svk-registered' to decide."
  (svn-svk-co-paths)
  (block nil
    (unless (file-exists-p file)
      (return nil))
    ;; Check file and each parent dir for svk-ness
    ;; Yeah, this is not the greatest. And it's UNIX-centric.
    (while (and file (not (string-equal file "/")))
      ;; For both SVK and file-name-directory, dirnames must not
      ;; include trailing /
      (setq file (substring file 0 (string-match "/\\'" file)))
      (if (svn-svk-assoc-string file svn-svk-co-paths)
          (return t)
        (setq file (file-name-directory file))))))

(defun svn-svk-co-path-of (file)
  "Return the CO path holding FILE, or nil."
  (car (find-if #'(lambda (codir)
                    (and (stringp codir)
                         (string-match (concat "^" codir) file)))
                svn-svk-co-paths
                :key 'first)))

;; --------------------------------------------------------------------------------
;; status persistent options
;; --------------------------------------------------------------------------------

(defun svn-svk-status-base-dir (&optional file)
  "Implementation of `svn-status-base-dir' for the SVK backend."
  (setq base-dir (or (and file (file-name-directory (concat file "/")))
                     (expand-file-name default-directory))))

(provide 'psvn-svk)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; psvn-svk.el ends here
