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

(add-to-list 'load-path dotfiles-dir)

(setq custom-file (concat dotfiles-dir "custom.el"))

;; Load up ELPA, the package manager
(require 'package)

(mapc '(lambda (item) (add-to-list 'package-archives item))
      '(("elpa" . "http://tromey.com/elpa/")
        ("technomancy" . "http://repo.technomancy.us/emacs/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)
(smex-initialize)
(load custom-file 'noerror)

(defun my/unique-strings (strings)
  "Takes a list of strings and returns the list with *adjacent*
duplicates removed."
  (let ((result '()))
    (while (consp strings)
      (if (not (string= (car strings) (car (cdr strings))))
          (setq result (cons (car strings) result)))
      (setq strings (cdr strings)))
    (nreverse result)))

;; Copy of debian-run-directories.
(defun my/run-directories (&rest dirs)
  "Load each file of the form XXfilename.el or XXfilename.elc in
any of the dirs, where XX must be a number. The files will be run
in alphabetical order. If a file appears in more than one of the
dirs, then the earlier dir takes precedence, and a .elc file
always supercedes a .el file of the same name."

  (let* ((paths dirs)
         ;; Get a list of all the files in all the specified
         ;; directories that match the pattern.
         (files
          (apply 'append
                 (mapcar
                  (lambda (dir)
                    (directory-files dir nil "^[0-9][0-9].*\\.elc?$" t))
                  paths)))

         ;; Now strip the directory portion, remove any .el or .elc
         ;; extension.

         (stripped-names
          (mapcar (lambda (file)
                    (if (string-match "\\.el$" file)
                        (substring file 0 -3)
                      (if (string-match "\\.elc$" file)
                          (substring file 0 -4)
                        file)))
                  (mapcar
                   (lambda (file) (file-name-nondirectory file))
                   files)))

         ;; Finally sort them, and delete duplicates
         (base-names (my/unique-strings (sort stripped-names 'string<)))

         (old-load-path load-path))

    ;; Set a new load path with the directories specified in the
    ;; proper order, and first.
    (let ((new-path (append paths load-path)))
      (setq load-path new-path)
      ;; Now load the files.  "load" will make sure we get the byte
      ;; compiled one first, if any, and will respect load-path's
      ;; ordering.
      (mapcar
       (lambda (file)
         (condition-case err
             (load file nil)
           (error (message "Error while loading %s: %s"
                           file (error-message-string err)))))
       base-names)
      ;; restore the old load-path -- including any new paths added by
      ;; files loaded in directory traversal.
      (let ((add-on-package-paths
             (delq nil (mapcar
                        (lambda (item)
                          (if (not (member item new-path))
                              item
                            nil))
                        load-path))))
        (setq load-path (append add-on-package-paths old-load-path))))))

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

(my/run-directories "~/.emacs.d/startup")

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

(defun server-edit-presets ()
  "Run some things when a server buffer is opened."
  (cond
   ;; When editing mail, set the goal-column to 72.
   ((string-match "mail\\.google\\.com\\.[0-9a-z]+\\.txt" (buffer-name))
    (org-mode)
    (auto-fill-mode)
    (setq fill-column 72)
    (save-excursion
      (goto-char (point-min))
      ;; Replace non-breaking strange space characters
      (while (search-forward (char-to-string 160) nil t)
        (replace-match " "))))))

(load-file (concat dotfiles-dir "/nxhtml/autostart.el"))

(c-add-style "drupal"
             '((c-basic-offset . 2)
               (c-offsets-alist . ((arglist-close . c-lineup-close-paren)
                                   (case-label . +)
                                   (arglist-intro . +)
                                   (arglist-cont-nonempty . c-lineup-math)))))

(defun c-style-drupal ()
  "Set the style to \"drupal\"."
  (interactive)
  (c-set-style "drupal"))

(add-hook 'ruby-mode-hook 'flyspell-prog-mode)

(highline-mode-on)

;; Re-enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; Allow "/sudo:host:/etc/stuff" to sudo on a remote host
(eval-after-load 'tramp
  '(progn
     (add-to-list 'tramp-default-proxies-alist
                  '(nil "\\`root\\'" "/ssh:%h:"))
     (add-to-list 'tramp-default-proxies-alist
                  '((regexp-quote (system-name)) nil nil))))

(add-to-list 'auto-mode-alist '("\\.json\\'" . javascript-mode))

(require 'undo-tree)
(global-undo-tree-mode)

(autoload 'ensime-scala-mode-hook "ensime" "Conveniance hook function that just starts ensime-mode.")
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(remove-hook 'esk-coding-hook 'esk-pretty-lambdas)

;;; init.el ends here
