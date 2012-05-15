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

(eval-after-load "semantic"
  '(require 'semantic/bovine/el))
