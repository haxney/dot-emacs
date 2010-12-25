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

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;; Load up ELPA, the package manager

(require 'package)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("technomancy" . "http://repo.technomancy.us/emacs/") t)
(package-initialize)


(load custom-file 'noerror)

;;; init.el ends here
