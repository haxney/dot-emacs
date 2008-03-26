;; CSS mode
(autoload 'css-mode "css-mode" "Enter CSS-mode." t)
(setq auto-mode-alist (cons '("\\.css$" . css-mode) auto-mode-alist))
(eval-after-load "css-mode"
  '(progn
     (setq cssm-indent-function 'cssm-c-style-indenter)
     (setq cssm-indent-level 4)))
