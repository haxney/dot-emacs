;;; 50planner.el --- Set up planner project.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: planner project local

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

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

	 (add-hook 'planner-mode-hook 'flyspell-mode)

	 ;; Set up planner template
	 (defun dhackney/planner-daily-template ()
	   "Build a new daily planner page."
	   (insert (concat "#title Journal Entry for "
					   (format-time-string "%A, %B %e, %Y")
					   "\n\n* Tasks\n\n\nWake: Up: \n\n* Events\n")))

	 (setq planner-day-page-template 'dhackney/planner-daily-template)))

;;; 50planner.el ends here
