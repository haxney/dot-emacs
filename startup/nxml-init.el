(eval-after-load "nxml"
  '(progn
     ;; Spelling in nXML
     (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)

     ;; Add new schemas to nXML
     (push (concat conf-home
				   (file-name-as-directory "schemas")
				   "schemas.xml")
		   rng-schema-locating-files-default)))
