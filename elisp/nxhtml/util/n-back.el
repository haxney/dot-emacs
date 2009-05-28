;;; n-back.el --- n-back game
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-05-23 Sat
(defconst n-back:version 0.5);; Version:
;; Last-Updated: 2009-05-27 Wed
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
  ;; `cl', `windmove', `winsav', `winsize'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; n-back game for brain training.  See `n-back' for more information.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'winsize) ;; Ehum...
;;(require 'new-key-seq-widget)

(defvar n-back-game-window nil)
(defvar n-back-game-buffer nil)

(defvar n-back-ctrl-window nil)
(defvar n-back-ctrl-buffer nil)

(defvar n-back-info-window nil)
(defvar n-back-info-buffer nil)

(defgroup n-back nil
  "Customizations for `n-back' game.
Bug: does not work without this line???"
  :group 'games)

(defcustom n-back-level 1
  "n-back level."
  :type '(radio (const 1)
                 (const 2)
                 (const 3)
                 (const 4))
  :set (lambda (sym val)
         (set-default sym val)
         (when (featurep 'n-back)
           (n-back-update-control-buffer)
           (n-back-update-info)))
  :group 'n-back)

(defcustom n-back-active-match-types '(position color sound)
  "Active match types."
  :type '(set (const position)
              (const color)
              (const sound)
              (const word))
  :set (lambda (sym val)
         (set-default sym val)
         (setq n-back-num-active (length val))
         (when (featurep 'n-back)
           (n-back-init-control-status)
           (n-back-update-control-buffer)
           (n-back-update-info)))
  :group 'n-back)

(defcustom n-back-allowed-match-types '(position color sound word)
  "Match types allowed in auto challenging."
  :type '(set (const position)
              (const color)
              (const sound)
              (const word))
  :set (lambda (sym val)
         (set-default sym val)
         (when (featurep 'n-back)
           (n-back-set-random-match-types (length n-back-active-match-types) nil)
           (n-back-init-control-status)
           (n-back-update-control-buffer)
           (n-back-update-info)))
  :group 'n-back)

(defun n-back-toggle-auto-challenge ()
  (interactive)
  (let ((val (not n-back-auto-challenge)))
    (customize-set-variable 'n-back-auto-challenge val)
    (customize-set-value 'n-back-auto-challenge val)))

(defcustom n-back-auto-challenge t
  "Automatic challenge decrease/increase."
  :type 'boolean
  :group 'n-back)

(defcustom n-back-colors
  '("gold" "orange red" "lawn green" "peru" "pink" "gray" "light blue")
  "Random colors to display."
  :type '(repeat color)
  :group 'n-back)

(defcustom n-back-words "you cat going me forest crying brown"
  "Random words to display."
  :type 'string
  :group 'n-back)

(defcustom n-back-sound-volume 0.2
  "Sound volume 0-1."
  :type 'float
  :group 'n-back)

(defvar n-back-sound-files nil)
;;(n-back-get-sound-files)
(defun n-back-get-sound-files ()
  (let ((dir (nth 0 n-back-sounds))
        (regexp (nth 1 n-back-sounds)))
    (setq n-back-sound-files (directory-files dir nil regexp))))

(defcustom n-back-sounds '("c:/program files/brain workshop/res" "piano-")
  "Random sounds location."
  :type '(list (directory :tag "Directory")
               (regexp :tag "File name regexp"))
  :group 'n-back)

(defun n-back-toggle-position ()
  (interactive)
  (n-back-toggle 'position))

(defun n-back-toggle-color ()
  (interactive)
  (n-back-toggle 'color))

(defun n-back-toggle-sound ()
  (interactive)
  (n-back-toggle 'sound))

(defun n-back-toggle-word ()
  (interactive)
  (n-back-toggle 'word))

(defun n-back-toggle (match-type)
  (n-back-toggle-1 match-type 'n-back-active-match-types))

(defun n-back-toggle-allowed-position ()
  (interactive)
  (n-back-toggle-allowed 'position))

(defun n-back-toggle-allowed-color ()
  (interactive)
  (n-back-toggle-allowed 'color))

(defun n-back-toggle-allowed-sound ()
  (interactive)
  (n-back-toggle-allowed 'sound))

(defun n-back-toggle-allowed-word ()
  (interactive)
  (n-back-toggle-allowed 'word))

(defun n-back-toggle-allowed (match-type)
  (n-back-toggle-1 match-type 'n-back-allowed-match-types))

(defun n-back-sort-types (types)
  (sort types
        (lambda (a b)
          (let ((all '(position color sound word)))
            (< (length (memq a all))
               (length (memq b all)))))))

(defun n-back-toggle-1 (match-type active-list-sym)
  (let (active-types)
    (if (memq match-type (symbol-value active-list-sym))
        (setq active-types (delq match-type (symbol-value active-list-sym)))
      (setq active-types (cons match-type (symbol-value active-list-sym))))
    (setq active-types (n-back-sort-types active-types))
    (customize-set-variable active-list-sym active-types)
    (customize-set-value active-list-sym active-types)))

(defvar n-back-control-mode-map nil)

(defun n-back-decrease-speed ()
  (interactive)
  (setq n-back-sec-per-trial (+ n-back-sec-per-trial 0.25))
  (when (> n-back-sec-per-trial 5.0)
    (setq n-back-sec-per-trial 5.0))
  (n-back-update-info))

(defun n-back-increase-speed ()
  (interactive)
  (let ((sec (- n-back-sec-per-trial 0.25)))
    (when (< sec 1.0)
      (setq sec 1.0))
    (customize-set-variable 'n-back-sec-per-trial sec)
    (customize-set-value 'n-back-sec-per-trial sec)))

(defun n-back-help ()
  (interactive)
  (describe-function 'n-back))

(defun n-back-change-level (level)
  (interactive (progn
                 (if (and (numberp last-input-event)
                          (>= last-input-event ?1)
                          (<= last-input-event ?9))
                     (list (- last-input-event ?0))
                   (list (string-to-int (read-string "Level: "))))))
  (customize-set-variable 'n-back-level level)
  (customize-set-value 'n-back-level level))

(defun n-back-make-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map [?1] 'n-back-change-level)
    (define-key map [?2] 'n-back-change-level)
    (define-key map [?3] 'n-back-change-level)
    (define-key map [?4] 'n-back-change-level)
    (define-key map [?5] 'n-back-change-level)
    (define-key map [?6] 'n-back-change-level)
    (define-key map [??] 'n-back-help)
    (define-key map [?\ ] 'n-back-play)
    (define-key map [(control ?g)] 'n-back-stop)
    (define-key map [?-] 'n-back-decrease-speed)
    (define-key map [?+] 'n-back-increase-speed)

    (define-key map [(control ?r)] 'n-back-reset-game-to-saved)
    (define-key map [(control ?s)] 'n-back-save-game-settings)

    (define-key map [?t ?p] 'n-back-toggle-position)
    (define-key map [?t ?c] 'n-back-toggle-color)
    (define-key map [?t ?s] 'n-back-toggle-sound)
    (define-key map [?t ?w] 'n-back-toggle-word)

    (define-key map [?T ?a] 'n-back-toggle-auto-challenge)
    (define-key map [up]    'n-back-challenge-up)
    (define-key map [down]  'n-back-challenge-down)

    (define-key map [?T ?p] 'n-back-toggle-allowed-position)
    (define-key map [?T ?c] 'n-back-toggle-allowed-color)
    (define-key map [?T ?s] 'n-back-toggle-allowed-sound)
    (define-key map [?T ?w] 'n-back-toggle-allowed-word)

    (define-key map (n-back-key-binding 'position) 'n-back-position-answer)
    (define-key map (n-back-key-binding 'color)    'n-back-color-answer)
    (define-key map (n-back-key-binding 'sound)    'n-back-sound-answer)
    (define-key map (n-back-key-binding 'word)     'n-back-word-answer)
    ;;(define-key map [t] 'ignore)
    (setq n-back-control-mode-map map)))

(defun n-back-key-binding (what)
  (nth
   (case what
    (position 0)
    (color    1)
    (sound    2)
    (word     3))
   n-back-keys))

(defcustom n-back-keys
  '(
    [?p]
    [?c]
    [?s]
    [?w]
    )
  "Key bindings for answering."
  :type '(list
          (key-sequence :tag "position key")
          (key-sequence :tag "color key")
          (key-sequence :tag "sound key")
          (key-sequence :tag "word key")
          )
  :set (lambda (sym val)
         (set-default sym val)
         (n-back-make-keymap))
  :group 'n-back)

(defcustom n-back-sec-per-trial 3.0
  "Seconds per trial."
  :type 'float
  :set (lambda (sym val)
         (set-default sym val)
         (when (featurep 'n-back)
           (n-back-update-info)))
  :group 'n-back)

(defvar n-back-frame nil)

;;;###autoload
(defun n-back ()
  "Start Emacs n-back game.
Just follow the on screen instructions to play the game.

This game is shamelessly modeled after Brain Workshop, see URL
`http://brainworkshop.sourceforge.net/'. Not all features there
are implemented here, but some new are maybe ...

The game is supposed to increase your working memory and fluid
intelligence.  The game resembles but it not the same as that
used in the report by Jaeggi mentioned at the above url.


-----
Below is a short excerpt from the report by Jaeggi et al which
gave the idea to the game:

Training task.  For the training task, we used the same material
as described by Jaeggi et al.  (33), which was a dual n-back task
where squares at eight different locations were presented
sequentially on a computer screen at a rate of 3 s (stimulus
length, 500 ms; interstimulus interval, 2,500 ms).
Simultaneously with the presentation of the squares, one of eight
consonants was presented sequentially through headphones.  A
response was required whenever one of the presented stimuli
matched the one presented n positions back in the sequence.  The
value of n was the same for both streams of stimuli.  There were
six auditory and six visual targets per block (four appearing in
only one modality, and two appearing in both modalities
simultaneously), and their positions were determined randomly.
Participants made responses manually by pressing on the letter
‘‘A’’ of a standard keyboard with their left index finger for
visual targets, and on the letter ‘‘L’’ with their right index
finger for auditory targets.  No responses were required for
non-targets."
  (interactive)
  (when window-system
    (unless (frame-live-p n-back-frame)
      (setq n-back-frame (make-frame
                          '((name . "n-back game")
                            (background-color . "cornsilk")))))
    (select-frame n-back-frame)
    (raise-frame n-back-frame))
  (n-back-cancel-timers)
  (n-back-init-control-status)
  (n-back-setup-windows)
  )

(defconst n-back-match-types
  '((position ": position match" nil)
    (color    ": color match" nil)
    (sound    ": sound match" nil)
    (word     ": word match" nil)
    ))

(defconst n-back-control-status nil)

;;(n-back-set-match-status 'position 'bad)
(defun n-back-set-match-status (match-type status)
  (unless (memq status '(ok bad nil)) (error "bad status=%s" status))
  (let ((entry (assoc match-type n-back-control-status)))
    (setcar (cddr entry) status)
    ))

;;(n-back-clear-match-status)
(defun n-back-clear-match-status ()
  (dolist (entry n-back-control-status)
    (setcar (cddr entry) nil)
    ))

;; (n-back-init-control-status)
(defun n-back-init-control-status ()
  (setq n-back-control-status nil)
  (dolist (what n-back-active-match-types)
    (setq n-back-control-status
          (cons (assoc what n-back-match-types)
                n-back-control-status))))

(defsubst n-back-is-playing ()
  (timerp n-back-timer))

;;(n-back-update-control-buffer)
(defun n-back-update-control-buffer ()
  (when (buffer-live-p n-back-ctrl-buffer)
    (with-current-buffer n-back-ctrl-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (propertize (format "%s %s-back"
                                  (let ((n (length n-back-active-match-types)))
                                    (cond
                                     ((= 1 n) "Single")
                                     ((= 2 n) "Dual")
                                     ((= 3 n) "Triple")
                                     ))
                                  n-back-level
                                  ) 'face '(:background "gold"))
              (propertize
               (if (n-back-is-playing) "  Press C-g to stop" "  Press SPACE to play")
               'face '(:foreground "blue"))
              (if (n-back-is-playing) (format "  Left %s" n-back-trials-left) "")
              "\n")
      ;;(unless n-back-control-status (n-back-init-control-status))
      (dolist (entry n-back-control-status)
        (let ((what (nth 0 entry))
              (msg  (nth 1 entry))
              (sts  (nth 2 entry))
              )
          (setq msg (concat (key-description (n-back-key-binding what)) msg))
          (cond
           ((eq sts 'bad)
            (setq msg (propertize msg 'face '(:foreground "red"))))
           ((eq sts 'ok)
            (setq msg (propertize msg 'face '(:foreground "green")))))
          (insert msg "   "))
        )
      (setq buffer-read-only t)
      (if (window-live-p n-back-ctrl-window)
          (with-selected-window n-back-ctrl-window
            (goto-char 1))
        (goto-char 1)))))

;;(n-back-compute-result-values n-back-result)
(defvar n-back-result-values nil)
(defun n-back-compute-single-result-value (entry)
  (let* ((what (nth 0 entry))
         (good (nth 1 entry))
         (bad  (nth 2 entry))
         (miss (nth 3 entry))
         (err (+ bad miss))
         ;;(tot  (+ good bad miss 0.0))
         ;;(gnum 6)
         ;;(weighted-err (* err (/ gnum tot)))
         )
    (cons what (/ (- n-back-trials err 0.0)
                  n-back-trials))))

(defun n-back-compute-result-values (result)
  (let ((results nil))
    (dolist (entry result)
      (let ((res (n-back-compute-single-result-value entry)))
        (setq results (cons res results))))
    (setq n-back-result-values (reverse results))))

;; Thresholds
(defun n-back-view-threshold-discussion-page ()
  (interactive)
  (browse-url "http://groups.google.com/group/brain-training/browse_thread/thread/f4bfa452943c2a2d/ba31adfd0b97771c?lnk=gst&q=threshold#ba31adfd0b97771c"))

(defvar n-back-num-active nil)

;;(n-back-set-next-challenge)
(defvar n-back-worst nil)

(defvar n-back-challenge-change nil)

(defun n-back-set-next-challenge ()
  (let ((r 2.0))
    (dolist (res n-back-result-values)
      (when (< (cdr res) r)
        (setq r (cdr res))
        (setq n-back-worst res))))
  (let ((worst-result (cdr n-back-worst)))
    (setq n-back-challenge-change (if (< worst-result 0.74)
                                      'down
                                    (if (> worst-result 0.91)
                                        'up
                                      'stay)))
    ;;(message "worst=%s, change=%s" worst-result change)
    (n-back-change-challenge n-back-challenge-change)))

(defun n-back-challenge-up ()
  (interactive)
  (n-back-change-challenge 'up))

(defun n-back-challenge-down ()
  (interactive)
  (n-back-change-challenge 'down))

(defun n-back-change-challenge (challenge-change)
  (let ((new-level n-back-level)
        (new-num-active n-back-num-active))
    (case challenge-change
      (down
       (if (= 1 n-back-num-active)
           (unless (= 1 n-back-level)
             (setq new-num-active 3)
             (setq new-level (1- n-back-level)))
         (setq new-num-active (1- n-back-num-active))))
      (up
       (if (or (<= 3 n-back-num-active)
               (<= (length n-back-allowed-match-types) n-back-num-active))
           (progn
             (setq new-level (1+ n-back-level))
             (setq new-num-active 1))
         (setq new-num-active (1+ n-back-num-active)))))
    (when (= new-level 0) (setq new-level 1))
    (when (= new-num-active 0) (setq new-num-active 1))
    (unless (= new-level n-back-level)
      (customize-set-variable 'n-back-level new-level)
      (customize-set-value 'n-back-level new-level))
    (n-back-set-random-match-types new-num-active (car n-back-worst))))

(defun n-back-set-random-match-types (num worst)
  (let ((alen (length n-back-allowed-match-types))
        types)
    (unless (<= num alen)
      (error "Too many match types required = %s" num))
    (when worst (add-to-list 'types worst))
    (while (< (length types) num)
      (add-to-list 'types (nth (random alen) n-back-allowed-match-types)))
    (setq types (n-back-sort-types types))
    (customize-set-variable 'n-back-active-match-types types)
    (customize-set-value 'n-back-active-match-types types)
    ))

(defcustom n-back-keybinding-color "OliveDrab1"
  "Background color for key binding hints."
  :type 'color
  :group 'n-back)

(defun n-back-update-info ()
  (when (buffer-live-p n-back-info-buffer)
    (when (window-live-p n-back-info-window)
      (set-window-buffer n-back-info-window n-back-info-buffer))
    (with-current-buffer n-back-info-buffer
    (setq buffer-read-only nil)
    (erase-buffer)

    (insert (propertize "n-back" 'face '(:background "gold"))
            "  "
            (propertize "Help: ?" 'face '(:background "OliveDrab1")))

    ;; Auto challenging
    (insert "\n\nAuto challenging: "
            (if n-back-auto-challenge "on " "off ")
            (propertize "toggle: Ta" 'face '(:background "Olivedrab1")))

    (insert "\n  Manually change challenging: "
            (propertize "up-arrow/down-arrow" 'face '(:background "Olivedrab1")))

    (insert "\n  Allowed match types: ")
    (dolist (type n-back-allowed-match-types)
      (insert (format "%s " type)))
    (insert (propertize "toggle: T" 'face '(:background "OliveDrab1")))

    ;; Current game
    (insert "\n\nCurrent game:")

    (insert (format "\n  Level: %s " n-back-level)
            (propertize "change: number 1-9" 'face '(:background "OliveDrab1")))
    (insert "\n  Match types: ")
    (dolist (type n-back-active-match-types)
      (insert (format "%s " type)))
    (insert (propertize "toggle: t" 'face '(:background "OliveDrab1")))

    (insert (format "\n  %.2f seconds per trial " n-back-sec-per-trial)
            (propertize "change: +/-" 'face '(:background "OliveDrab1")))

    ;; Save and restore
    (insert "\n\n")
    (insert "Game settings: "
            (propertize "reset: C-r" 'face '(:background "Olivedrab1"))
            " "
            (propertize "save: C-s" 'face '(:background "Olivedrab1"))
            )

    (insert "\n\n")
    (unless (or (n-back-is-playing)
                (not n-back-result))
      (insert (propertize (format "Last result, %s" n-back-challenge-change)
                          'face '(:background "yellow"))
              "\n  Good-Bad-Miss:")
      (dolist (entry n-back-result)
        (let* ((what (nth 0 entry))
               (good (nth 1 entry))
               (bad  (nth 2 entry))
               (miss (nth 3 entry))
               (tot  (+ good bad miss 0.0))
               (res (n-back-compute-single-result-value entry)))
          (insert (format "  %s: %s-%s-%s (%d%%)"
                          (key-description (n-back-key-binding what))
                          good
                          bad
                          miss
                          (floor (* 100 (cdr res))))))))

    (setq buffer-read-only t))))

(defun n-back-show-welcome ()
  (with-current-buffer n-back-game-buffer
    (let ((src "c:/program files/brain workshop/res/brain_graphic.png")
          img
          buffer-read-only)
      (insert (propertize "\nEmacs n-back game (after Brain Workshop)\n\n" 'face '(:height 2.0)))
      (if (file-exists-p src)
          (condition-case err
              (setq img (create-image src nil nil
                                      :relief 1
                                      ;;:margin inlimg-margins
                                      ))
            (error (setq img (error-message-string err))))
        (setq img (concat "Image not found: " src)))
      (if (stringp img)
          (insert img)
        (insert-image img)))))

(defun n-back-setup-windows ()
  (delete-other-windows)
  (set-window-dedicated-p (selected-window) nil)
  ;; Info
  (split-window-horizontally)
  (setq n-back-info-window (next-window (frame-first-window)))
  (setq n-back-info-buffer (get-buffer-create "* n-back info *"))
  (when (< 60 (window-width n-back-info-window))
    (with-selected-window n-back-info-window
      (enlarge-window (- 60 (window-width n-back-info-window)) t)))
  (with-current-buffer n-back-info-buffer
    (n-back-control-mode)
    (setq wrap-prefix "      "))
  (n-back-update-info)
  ;; Control
  (split-window-vertically)
  (setq n-back-ctrl-window (next-window (frame-first-window)))
  (setq n-back-ctrl-buffer (get-buffer-create "* n-back control *"))
  (set-window-buffer n-back-ctrl-window n-back-ctrl-buffer)
  (with-current-buffer n-back-ctrl-buffer (n-back-control-mode))
  (n-back-update-control-buffer)
  (fit-window-to-buffer n-back-ctrl-window)
  (set-window-dedicated-p n-back-ctrl-window t)
  ;; Game
  (setq n-back-game-window (frame-first-window))
  (setq n-back-game-buffer (get-buffer-create "*n-back game*"))
  (set-window-buffer n-back-game-window n-back-game-buffer)
  (set-window-dedicated-p n-back-game-window t)
  (n-back-clear-game-window)
  (n-back-show-welcome)
  (with-current-buffer n-back-game-buffer (n-back-control-mode))
  ;; Position in control window
  (select-window n-back-ctrl-window)
  )

;;(n-back-display "str" 1 0 3 3 6)
(defun n-back-display (str x y cols rows max-strlen color)
  ;;(message "(n-back-display %s %s %s %s %s %s %s)" str x y cols rows max-strlen color)
  ;; (unless (and (window-live-p n-back-game-window)
  ;;              (eq (window-frame n-back-game-window) (selected-frame))
  ;;              )
  ;;   (n-back-setup-windows))
  (unless (< x cols) (error "not x=%s < cols=%s" x cols))
  ;;(unless (< y rows) (error "not y=%s < rows=%s" y rows))
  (with-current-buffer n-back-game-buffer
    (let* ((tot-str "")
           ;; Pad spaces left, two right, four between
           (game-w (window-width n-back-game-window))
           (pad-x 0)
           (scale (if (not window-system)
                      1.0
                    (/ (* 1.0 game-w)
                       (+ (* 2 pad-x)
                          (* (1- cols) 4)
                          (* cols max-strlen)))))
           (str-diff (- max-strlen (length str)))
           (str-l-len (/ str-diff 2))
           (str-r-len (- max-strlen (length str) str-l-len))
           (face-spec (if window-system
                          (list :background color :height scale)
                        (list :background color)))
           (str-disp (propertize
                      (concat (make-string str-l-len 32) str (make-string str-r-len 32))
                      'face face-spec))
           (col-str (concat
                     (make-string pad-x ?p)
                     (make-string
                      (+ (* x (+ 4 max-strlen)))
                      32
                      ;;?x
                      )))
           ;; Pad lines above and below, two between
           (pad-y 0)
           (game-h (window-height n-back-game-window))
           (game-h-scaled (/ game-h scale))
           (lines-between (/ (- game-h-scaled rows (* 2 pad-y))
                             (1- rows)))
           (row-str (make-string (+ pad-x
                                    (floor (* y (1+ lines-between))))
                                 ?\n))
           )
      ;;(message "scale=%s, game-w=%s, colstr='%s', lines-between=%s" scale game-w col-str lines-between)
      (setq show-trailing-whitespace nil)
      (setq cursor-type nil)
      (erase-buffer)
      (setq tot-str row-str)
      (setq tot-str (concat tot-str col-str))
      ;;(setq tot-str (concat tot-str str-disp))
      ;;(message "len tot-str=%s" (length tot-str))
      (insert (propertize tot-str 'face (list :height scale)))
      (insert str-disp)
      )))

;; (setq timer-list nil)
;;(n-back-display-in-timer)
;; (setq n-back-trials-left 3)

(defun n-back-clear-game-window ()
  (with-current-buffer n-back-game-buffer
    (erase-buffer)))

(defun n-back-play ()
  (interactive)
  (message "=== n-back game started ==========================")
  ;;(n-back-setup-windows)
  (unless n-back-active-match-types
    (error "No active match types"))
  (when (memq 'sound n-back-active-match-types) (n-back-get-sound-files))
  ;;(setq n-back-result nil)
  (n-back-init-control-status)
  (setq n-back-this-result nil)
  (n-back-cancel-timers)
  (n-back-start-main-timer)
  (n-back-update-control-buffer)
  )

(defun n-back-display-in-timer ()
  ;;(message "n-back-trials-left=%s" n-back-trials-left)
  (condition-case nil
      (progn
        (n-back-add-result)
        (if (>= 0 (setq n-back-trials-left (1- n-back-trials-left)))
            (progn
              (n-back-cancel-timers)
              (fit-window-to-buffer n-back-ctrl-window)
              (setq n-back-result n-back-this-result)
              (n-back-compute-result-values n-back-result)
              (when n-back-auto-challenge (n-back-set-next-challenge))
              (n-back-update-info)
              (n-back-init-control-status)
              (n-back-clear-match-status)
              (n-back-update-control-buffer)
              (message "Game over"))
          (let* ((use-position (memq 'position n-back-active-match-types))
                 (use-color (memq 'color n-back-active-match-types))
                 (use-sound (memq 'sound n-back-active-match-types))
                 (use-word  (memq 'word  n-back-active-match-types))
                 (old-rec (when (> (ring-length n-back-ring) n-back-level)
                            (ring-ref n-back-ring (1- n-back-level))))
                 (cols 3)
                 (rows 3)
                 (x (if use-position (random 3) 1))
                 (y (if use-position (random 3) 1))
                 (old-x (if use-position (nth 1 old-rec)))
                 (old-y (if use-position (nth 2 old-rec)))
                 (color (nth (if use-color (random (length n-back-colors)) 0) n-back-colors))
                 (old-color (if use-color (nth 3 old-rec)))
                 (sound (when use-sound (expand-file-name (nth (random (length n-back-sound-files))
                                                               n-back-sound-files)
                                                          (nth 0 n-back-sounds))))
                 (old-sound (if use-sound (nth 4 old-rec)))
                 (words (when use-word (split-string n-back-words)))
                 (word (when use-word (nth (random (length words)) words)))
                 (old-word (when use-word (nth 5 old-rec)))
                 (str (if word word "")) ;(format "%s" n-back-trials-left))
                 (max-strlen (if words
                                 (+ 2 (apply 'max (mapcar (lambda (w) (length w)) words)))
                               5))
                 (compensate 24)
                 )
            ;; To get more targets make it more plausible that it is the same here.
            ;; (/ (- 6 (/ 20.0 8)) 20)
            (when old-rec
              (when (and use-position
                         (not (and (= x old-x)
                                   (= y old-y)))
                         (< (random 100) compensate))
                (setq x (nth 1 old-rec))
                (setq y (nth 2 old-rec)))
              (when (and use-color
                         (not (equal color old-color))
                         (< (random 100) compensate))
                (setq color (nth 3 old-rec)))
              (when (and use-sound
                         (not (equal sound old-sound))
                         (< (random 100) compensate))
                (setq sound (nth 4 old-rec)))
              (when (and use-word
                         (not (equal word old-word))
                         (< (random 100) compensate))
                (setq word (nth 5 old-rec))))
            (setq str word) ;; fix-me
            (ring-insert n-back-ring (list str x y color sound word))
            (n-back-display str x y cols rows max-strlen color)
            ;;(when sound (play-sound (list 'sound :file sound)))
            (n-back-clear-match-status)
            (n-back-update-control-buffer)
            (setq n-back-clear-timer (run-with-timer 0.5 nil 'n-back-clear-game-window))
            (when sound (run-with-timer 0.01 nil 'n-back-play-sound-in-timer sound))
            )))
    ((debug error)
     nil)))

(defun n-back-play-sound-in-timer (sound-file)
  (condition-case nil
      (play-sound (list 'sound :file sound-file :volume n-back-sound-volume))
    ((debug error)
     nil)))


;;; Answers

(defvar n-back-result nil)
(defvar n-back-this-result nil)
;;(defvar n-back-answers nil)

(defun n-back-add-result ()
  "Add result of last trial."
  (when (= (ring-length n-back-ring) (1+ n-back-level))
    ;;((color "F: color match" nil) (position "A: position match" nil))
    ;;(message "n-back-add-result: n-back-control-status=%s" n-back-control-status)
    (dolist (sts-entry n-back-control-status)
      (let* ((what (nth 0 sts-entry))
             (sts  (nth 2 sts-entry))
             (matches (n-back-matches what))
             (num (cond
                   ((eq sts 'ok) 1)
                   ((eq sts 'bad) 2)
                   ((eq sts nil) (when matches 3))
                   (t (error "bad status=%s" sts))))
             (res-entry (when num (assoc what n-back-this-result)))
             (lst (when num (nthcdr num res-entry))))
        (when num
          ;;(message "n-back-add-result: %s %s %s" what sts num)
          (if res-entry
              (setcar lst (1+ (car lst)))
            (setq res-entry (list what 0 0 0))
            (setq lst (nthcdr num res-entry))
            (setq n-back-this-result (cons res-entry n-back-this-result))))))))

(defun n-back-matches-position ()
    (let* ((comp-item (ring-ref n-back-ring n-back-level))
           (curr-item (ring-ref n-back-ring 0))
           (comp-x (nth 1 comp-item))
           (curr-x (nth 1 curr-item))
           (comp-y (nth 2 comp-item))
           (curr-y (nth 2 curr-item)))
      (and (= comp-y curr-y)
           (= comp-x curr-x))))

(defun n-back-matches-color ()
  (let* ((comp-item (ring-ref n-back-ring n-back-level))
         (curr-item (ring-ref n-back-ring 0))
         (comp-color (nth 3 comp-item))
         (curr-color (nth 3 curr-item)))
    (equal comp-color curr-color)))

(defun n-back-matches-sound ()
    (let* ((comp-item (ring-ref n-back-ring n-back-level))
           (curr-item (ring-ref n-back-ring 0))
           (comp-sound (nth 4 comp-item))
           (curr-sound (nth 4 curr-item)))
      (equal comp-sound curr-sound)))

(defun n-back-matches-word ()
    (let* ((comp-item (ring-ref n-back-ring n-back-level))
           (curr-item (ring-ref n-back-ring 0))
           (comp-word (nth 5 comp-item))
           (curr-word (nth 5 curr-item)))
      ;;(message "n-back-matches-word: curr=%s, comp=%s" curr-item comp-item)
      (equal comp-word curr-word)))

(defun n-back-matches (what)
  (cond
   ((eq what 'position) (n-back-matches-position))
   ((eq what 'color) (n-back-matches-color))
   ((eq what 'sound) (n-back-matches-sound))
   ((eq what 'word)  (n-back-matches-word))
   (t (error "Unknown match type: %s" what))))

(defun n-back-position-answer ()
  (interactive)
  ;;(message "n-back-position-answer here a, ring-size=%s" (ring-size n-back-ring))
  (when (and (memq 'position n-back-active-match-types)
             (> (ring-length n-back-ring) n-back-level))
    ;;(message "n-back-position-answer here b")
    (let ((sts (if (n-back-matches-position) 'ok 'bad)))
      (n-back-set-match-status 'position sts)
      (n-back-update-control-buffer))))

(defun n-back-color-answer ()
  (interactive)
  (when (and (memq 'color n-back-active-match-types)
             (> (ring-length n-back-ring) n-back-level))
    (let ((sts (if (n-back-matches-color) 'ok 'bad)))
      (n-back-set-match-status 'color sts)
      (n-back-update-control-buffer))))

(defun n-back-sound-answer ()
  (interactive)
  (when (and (memq 'sound n-back-active-match-types)
             (> (ring-length n-back-ring) n-back-level))
    (let ((sts (if (n-back-matches-sound) 'ok 'bad)))
      (n-back-set-match-status 'sound sts)
      (n-back-update-control-buffer))))

(defun n-back-word-answer ()
  (interactive)
  (when (and (memq 'word n-back-active-match-types)
             (> (ring-length n-back-ring) n-back-level))
    (let ((sts (if (n-back-matches-word) 'ok 'bad)))
      (n-back-set-match-status 'word sts)
      (n-back-update-control-buffer))))

(defun n-back-stop ()
  (interactive)
  (n-back-cancel-timers)
  (n-back-update-control-buffer)
  (message "Stopped n-back game")
  (with-current-buffer n-back-game-buffer
    (let ((buffer-read-only))
      (erase-buffer)
      (insert "Stopped"))))

(define-derived-mode n-back-control-mode nil "N-back"
  "Mode for controling n-back game."
  (setq cursor-type nil)
  (set (make-local-variable 'viper-emacs-state-mode-list) '(n-back-control-mode))
  (set (make-local-variable 'viper-emacs-state-hook) nil) ;; invis cursor
  (abbrev-mode -1)
  (setq show-trailing-whitespace nil)
  (visual-line-mode 1)
  (n-back-make-keymap))

(defvar n-back-trials-left nil)
(defvar n-back-timer nil)
(defvar n-back-clear-timer nil)

(defun n-back-cancel-timers ()
  (when (timerp n-back-timer)
    (cancel-timer n-back-timer))
  (setq n-back-timer nil)
  (when (timerp n-back-clear-timer)
    (cancel-timer n-back-clear-timer))
  (setq n-back-clear-timer nil)
  (winsize-set-mode-line-colors nil))

(defcustom n-back-trials 20
  "Number of trials per session."
  :type 'integer
  :group 'n-back)

(defvar n-back-ring nil)
(defun n-back-start-main-timer ()
  (n-back-cancel-timers)
  (winsize-set-mode-line-colors t)
  (setq n-back-ring (make-ring (1+ n-back-level)))
  (with-current-buffer n-back-game-buffer (erase-buffer))
  (setq n-back-trials-left (+ n-back-trials n-back-level 1))
  (random t)
  (setq n-back-timer
        (run-with-timer
         n-back-sec-per-trial
         n-back-sec-per-trial
         'n-back-display-in-timer)))

(defvar n-back-game-settings-symbols
    '(
      ;;n-back-keys
      n-back-level
      n-back-active-match-types
      n-back-allowed-match-types
      n-back-auto-challenge
      n-back-colors
      n-back-words
      ;;n-back-sound-volume
      ;;n-back-sounds
      n-back-sec-per-trial
      ;;n-back-keybinding-color
      ;;n-back-trials
      ))

(defun n-back-save-game-settings ()
  (interactive)
  (dolist (var n-back-game-settings-symbols)
  )
  (custom-save-all))

(defun n-back-reset-game-to-saved ()
  (interactive)
  (dolist (pass '(1 2))
    (dolist (var n-back-game-settings-symbols)
      (if (= pass 1)
          ;; pass 1 is for my lousy programming:
          (condition-case err
              (custom-reevaluate-setting var)
            (error nil))
        (custom-reevaluate-setting var)))))

(provide 'n-back)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; n-back.el ends here
