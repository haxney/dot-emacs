(defvar *emacs-load-start* (current-time))

;; This gives us `third'
(require 'cl)

(setq conf-home (concat (file-name-as-directory (expand-file-name "~"))
						(file-name-as-directory ".emacs.d")))

(setq conf-tmp (concat conf-home
                       (file-name-as-directory "tmp")))

;; Add to the end of the list
(add-to-list 'load-path (concat conf-home "elisp") t)

;; Store custom settings in a different file.
(setq custom-file
	  (concat conf-home "my-custom.el"))
(load custom-file 'noerror)

;; Automagically byte-compile loaded files. Apparently it needs to be done after
;; loading custom.
(require 'byte-code-cache)

;; ---- Package
;; Load package before loading startup files, since some of them may depend on
;; package being loaded
(require 'package)
(package-initialize)

;; Load all files in ~/.emacs.d/startup"
(let ((startup-files (directory-files
					  (concat conf-home
							  (file-name-as-directory "startup")) t "\.el$")))
  (mapc (lambda (file)
          (message "Loading startup file %s" file)
          (load file))
        startup-files))

;; Don't wrap lines, truncate them instead, but not for term mode
(setq-default truncate-lines t)
(add-hook 'term-mode-hook
          '(lambda () (setq truncate-lines nil)))

;; Load Pabbrev
(require 'pabbrev)
(global-pabbrev-mode)
;; do we need to disable it in term mode?

;; don't clutter directories!
(setq auto-save-directory (concat conf-tmp "autosave"))

;; create a backup file directory
(defun make-backup-file-name (file)
  (concat conf-tmp
		  (file-name-as-directory "baks")
		  (file-name-nondirectory file)
		  "~"))

;; Place semantic.cache files somewhere central
(setq semanticdb-default-save-directory
	  (concat conf-tmp "semantic-cache"))

;; ---- AUCTeX
(add-hook 'LaTeX-mode-hook 'flyspell-mode)


;; Time how long it took to start up.
(let ((the-time (current-time)))
  (message "My .emacs loaded in %dms"
           (/ (-
               (+
                (third the-time)
                (* 1000000
                   (second the-time)))
               (+
                (third *emacs-load-start*)
                (* 1000000
                   (second *emacs-load-start*)))
               ) 1000)))
