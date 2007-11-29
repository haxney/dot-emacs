;; mmm-mode
(autoload 'mmm-mode "mmm-mode" "Mulitple Minor Mode" t)

(eval-after-load "mmm-mode"
  '(progn
	 (setq mmm-global-mode 'maybe)
	 (setq mmm-submode-decoration-level 2)
	 (set-face-background 'mmm-code-submode-face  "lavender")
	 (set-face-background 'mmm-output-submode-face  "honeydew")
	 (set-face-background 'mmm-comment-submode-face  "tomato") ;delicious colours

	 (mmm-add-classes
	  '((embedded-ruby
		 :submode ruby-mode
		 :face mmm-code-submode-face
		 :front "<%[=#]?"
		 :back "%>"
		 :insert ((?r eruby-directive nil @ "<%" @ " " _ " " @ "%>" @)
				  (?= eruby-directive nil @ "<%=" @ " " _ " " @ "%>" @)))))
	 (mmm-add-classes
	  '((embedded-css
		 :submode css-mode
		 :face mmm-declaration-submode-face
		 :front "style=\""
		 :back "\"")))
	 (mmm-add-classes
	  '((embedded-javascript
		 :submode c-mode ;; javascript-generic-mode
		 :face mmm-declaration-submode-face
		 :front "\]*>"
		 :back "")))
	 (mmm-add-classes
	  '((embedded-javascript-attribute
		 :submode c-mode ;; javascript-generic-mode
		 :face mmm-declaration-submode-face
		 :front "\\bon\\w+=\\s-*\""
		 :back "\"")))


	 ;; What features should be turned on in this html-mode?
	 (add-to-list 'mmm-mode-ext-classes-alist
				  '(html-mode nil embedded-css))
	 (add-to-list 'mmm-mode-ext-classes-alist
				  '(html-mode nil embedded-ruby))
	 (add-to-list 'mmm-mode-ext-classes-alist
				  '(html-mode nil embedded-javascript))
	 (add-to-list 'mmm-mode-ext-classes-alist
				  '(html-mode nil embedded-javascript-attribute))

	 (global-set-key [f8] 'mmm-parse-buffer))