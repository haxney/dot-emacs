;;; local-emacs-next-compat.el --- Compatibility with Emacs 24.3

;; Copyright (C) 2012 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: convenience files local

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

;;; Commentary:
;;
;; Indentation and highlighting settings for upcoming Emacs versions.

;;; Code:

(put 'with-package-test 'lisp-indent-function 1)
(put 'package-with-cd 'lisp-indent-function 1)

(defconst lisp-font-lock-keywords-1
  (eval-when-compile
    `(;; Definitions.
      (,(concat "(\\(\\(?:ert-\\|cl-\\)?def\\("
                ;; Function declarations.
                "\\(advice\\|alias\\|generic\\|macro\\*?\\|method\\|test\\|"
                "setf\\|subst\\*?\\|un\\*?\\|"
                "ine-\\(condition\\|"
                "\\(?:derived\\|\\(?:global\\(?:ized\\)?-\\)?minor\\|generic\\)-mode\\|"
                "method-combination\\|setf-expander\\|skeleton\\|widget\\|"
                "function\\|\\(compiler\\|modify\\|symbol\\)-macro\\)\\)\\|"
                ;; Variable declarations.
                "\\(const\\(ant\\)?\\|custom\\|varalias\\|face\\|parameter\\|var\\)\\|"
                ;; Structure declarations.
                "\\(class\\|group\\|theme\\|package\\|struct\\|type\\)"
                "\\)\\)\\>"
                ;; Any whitespace and defined object.
                "[ \t'\(]*"
                "\\(setf[ \t]+\\sw+\\|\\sw+\\)?")
       (1 font-lock-keyword-face)
       (9 (cond ((match-beginning 3) font-lock-function-name-face)
                ((match-beginning 6) font-lock-variable-name-face)
                (t font-lock-type-face))
          nil t))
      ;; Emacs Lisp autoload cookies.  Supports the slightly different
      ;; forms used by mh-e, calendar, etc.
      ("^;;;###\\([-a-z]*autoload\\)" 1 font-lock-warning-face prepend)
      ;; Regexp negated char group.
      ("\\[\\(\\^\\)" 1 font-lock-negation-char-face prepend)))
  "Subdued level highlighting for Lisp modes.")

(defconst lisp-font-lock-keywords-2
  (append lisp-font-lock-keywords-1
   (eval-when-compile
     `(;; Control structures.  Emacs Lisp forms.
       (,(concat
          "(" (regexp-opt
               '("cond" "if" "while" "while-no-input" "let" "let*" "letrec"
                 "prog" "progn" "progv" "prog1" "prog2" "prog*"
                 "inline" "lambda" "save-restriction" "save-excursion"
                 "save-selected-window" "save-window-excursion"
                 "save-match-data" "save-current-buffer"
                 "combine-after-change-calls" "unwind-protect"
                 "condition-case" "condition-case-unless-debug"
                 "track-mouse" "eval-after-load" "eval-and-compile"
                 "eval-when-compile" "eval-when" "eval-next-after-load"
                 "with-case-table" "with-category-table"
                 "with-current-buffer" "with-demoted-errors"
                 "with-electric-help"
                 "with-local-quit" "with-no-warnings"
                 "with-output-to-string" "with-output-to-temp-buffer"
                 "with-selected-window" "with-selected-frame"
                 "with-silent-modifications" "with-syntax-table"
                 "with-temp-buffer" "with-temp-file" "with-temp-message"
                 "with-timeout" "with-timeout-handler" "with-wrapper-hook") t)
          "\\>")
          .  1)
       ;; Control structures.  Common Lisp forms.
       (,(concat
          "(" (regexp-opt
               '("when" "unless" "case" "ecase" "typecase" "etypecase"
                 "ccase" "ctypecase" "handler-case" "handler-bind"
                 "restart-bind" "restart-case" "in-package"
                 "break" "ignore-errors"
                 "loop" "do" "do*" "dotimes" "dolist" "cl-dolist" "the" "locally"
                 "proclaim" "declaim" "declare" "symbol-macrolet" "letf"
                 "lexical-let" "lexical-let*" "flet" "labels" "compiler-let"
                 "destructuring-bind" "macrolet" "tagbody" "block" "go"
                 "multiple-value-bind" "multiple-value-prog1"
                 "return" "return-from"
                 "with-accessors" "with-compilation-unit"
                 "with-condition-restarts" "with-hash-table-iterator"
                 "with-input-from-string" "with-open-file"
                 "with-open-stream" "with-output-to-string"
                 "with-package-iterator" "with-simple-restart"
                 "with-slots" "with-standard-io-syntax") t)
          "\\>")
          . 1)
       ;; Exit/Feature symbols as constants.
       (,(concat "(\\(catch\\|throw\\|featurep\\|provide\\|require\\)\\>"
                 "[ \t']*\\(\\sw+\\)?")
        (1 font-lock-keyword-face)
        (2 font-lock-constant-face nil t))
       ;; Erroneous structures.
       ("(\\(abort\\|assert\\|warn\\|check-type\\|cerror\\|error\\|signal\\)\\>" 1 font-lock-warning-face)
       ;; Words inside \\[] tend to be for `substitute-command-keys'.
       ("\\\\\\\\\\[\\(\\sw+\\)\\]" 1 font-lock-constant-face prepend)
       ;; Words inside `' tend to be symbol names.
       ("`\\(\\sw\\sw+\\)'" 1 font-lock-constant-face prepend)
       ;; Constant values.
       ("\\<:\\sw+\\>" 0 font-lock-builtin-face)
       ;; ELisp and CLisp `&' keywords as types.
       ("\\<\\&\\sw+\\>" . font-lock-type-face)
       ;; ELisp regexp grouping constructs
       ((lambda (bound)
          (catch 'found
            ;; The following loop is needed to continue searching after matches
            ;; that do not occur in strings.  The associated regexp matches one
            ;; of `\\\\' `\\(' `\\(?:' `\\|' `\\)'.  `\\\\' has been included to
            ;; avoid highlighting, for example, `\\(' in `\\\\('.
            (while (re-search-forward "\\(\\\\\\\\\\)\\(?:\\(\\\\\\\\\\)\\|\\((\\(?:\\?[0-9]*:\\)?\\|[|)]\\)\\)" bound t)
              (unless (match-beginning 2)
                (let ((face (get-text-property (1- (point)) 'face)))
                  (when (or (and (listp face)
                                 (memq 'font-lock-string-face face))
                            (eq 'font-lock-string-face face))
                    (throw 'found t)))))))
        (1 'font-lock-regexp-grouping-backslash prepend)
        (3 'font-lock-regexp-grouping-construct prepend))
;;;  This is too general -- rms.
;;;  A user complained that he has functions whose names start with `do'
;;;  and that they get the wrong color.
;;;      ;; CL `with-' and `do-' constructs
;;;      ("(\\(\\(do-\\|with-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face)
      )))
  "Gaudy level highlighting for Lisp modes.")

;;; local-emacs-next-compat.el ends here
