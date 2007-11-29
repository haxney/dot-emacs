;; CSS mode
(autoload 'css-mode "css-mode" "Enter CSS-mode." t)
(setq auto-mode-alist (cons '("\\.css$" . css-mode) auto-mode-alist))