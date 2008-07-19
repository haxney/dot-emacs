;;; 50planner.el --- Set up planner project.

;; Copyright (C) 2008, Daniel Hackney

;; Description: Set up planner project.
;; Author: Daniel Hackney
;; Maintainer: Daniel Hackney
;; Keywords: planner project local

;; This file is NOT part of GNU Emacs.

;;; Code:

(eval-after-load 'planner
  '(progn
	 (setq planner-project "Journal of Hax")
	 (setq muse-project-alist
		   '(("Journal of Hax"
			  ("~/Journal" ;; where your Planner pages are located
			   :default "Index" ;; use value of `planner-default-page'
			   :major-mode planner-mode
			   :visit-link planner-visit-link)

			  ;; This next part is for specifying where Planner pages
			  ;; should be published and what Muse publishing style to
			  ;; use.  In this example, we will use the XHTML publishing
			  ;; style.

			  (:base "planner-xhtml"
					 ;; where files are published to
					 ;; (the value of `planner-publishing-directory', if
					 ;;  you have a configuration for an older version
					 ;;  of Planner)
			   :path "~/Journal/html"))))

	 (autoload 'muse-project-publish "planner-publish" "Publish planner project" t)

	 ;; Include remember
	 (autoload 'remeber "remember-planner" "Remember mode" t)
	 (setq remember-handler-functions '(remember-planner-append))
	 (setq remember-annotation-functions planner-annotation-functions)

	 ;; Bind remember to C-c C-n
	 (global-unset-key (kbd "C-c C-n"))
	 (global-set-key (kbd "C-c C-n") 'remember)

	 (add-hook 'planner-mode-hook 'flyspell-mode)

	 ;; Set up planner template
	 (defun dhackney/planner-daily-template ()
	   "Build a new daily planner page."
	   (insert (concat "#title Journal Entry for "
					   (format-time-string "%A, %B %e, %Y")
					   "\n\n* Tasks\n\n\nWake: Up: \n\n* Events\n")))

	 (setq planner-day-page-template 'dhackney/planner-daily-template)))

;;; 50planner.el ends here
