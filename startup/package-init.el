;; Package -- Packaging system for Emacs.

(autoload 'package-list-packages "package" "List package files" t)

(eval-after-load 'package
  '(package-initialize))