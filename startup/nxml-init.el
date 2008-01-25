(eval-after-load "nxml-mode"
  '(progn
     ;; Spelling in nXML
     (require 'flyspell)
     (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)

     ;; Add new schemas to nXML
     (push (concat conf-home
 				   (file-name-as-directory "schemas")
 				   "schemas.xml")
 		   rng-schema-locating-files-default)))
