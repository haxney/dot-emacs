;(smex-initialize)

(when (require 'diminish nil 'noerror)
  (eval-after-load "company"
      '(diminish 'company-mode "Cmp"))
  (eval-after-load "abbrev"
    '(diminish 'abbrev-mode "Ab"))
  (eval-after-load "yasnippet"
    '(diminish 'yas/minor-mode "Y"))
  (eval-after-load "paredit"
    '(diminish 'paredit-mode "Par")))

(if (file-readable-p "~/Private/private.el")
    (load-file "~/Private/private.el"))

(eval-after-load "semantic"
  '(require 'semantic/bovine/el))
