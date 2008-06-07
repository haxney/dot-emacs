;; Place autosave files in

(defvar autosave-dir
  "~/.emacs.d/tmp/autosave/"
  "The directory in which to place auto-save (i.e. #foo#) files.")

(defun auto-save-file-name-p (filename)
  "Return non-nil if filename can be yielded by `make-auto-save-file-name'.
filename should lack slashes."
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))
