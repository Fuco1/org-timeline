;;; org-timeline.el --- Add graphical view of agenda to agenda buffer. -*- lexical-binding: t -*-

;; Copyright (C) 2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.3.0
;; Created: 16th April 2017
;; Package-requires: ((dash "2.13.0") (emacs "24.3"))
;; Keywords: calendar
;; URL: https://github.com/Fuco1/org-timeline/

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Add graphical view of agenda to agenda buffer.

;; This package adds a graphical view of the agenda after the last
;; agenda line.  By default the display starts at 5 AM today and
;; goes up to 4 AM next day (this covers 24 hours).

;; Scheduled tasks or tasks with time ranges are rendered in the
;; display with `org-timeline-block' face.  Clocked entires are
;; displayed in `org-timeline-clocked' face.  The background of
;; timeslots which are in the past is highlighted with
;; `org-timeline-elapsed' face.

;; You can use custom color for a task by adding the property
;; `TIMELINE_FACE' with either a string which is a color name or a
;; list which specifies the face properties or a symbol which is
;; taken to be a face name.

;;; Code:

(require 'dash)

(require 'org-agenda)

(defgroup org-timeline ()
  "Graphical view of agenda in agenda buffer."
  :group 'org
  :prefix "org-timeline-")

(defgroup org-timeline-faces ()
  "Faces for org-timeline."
  :group 'org-timeline)

(defcustom org-timeline-prepend nil
  "Option to prepend the timeline to the agenda."
  :type 'boolean
  :group 'org-timeline)

(defcustom org-timeline-show-clocked t
  "Option to show or hide clocked items."
  :type 'boolean
  :group 'org-timeline)

(defcustom org-timeline-dedicated-clocked-line t
  "Option to show clocked items in a dedicated line with 'group-name' '$'."
  :type 'boolean
  :group 'org-timeline)

(defcustom org-timeline-overlap-in-new-line nil
  "Option to create new lines for blocks that would otherwise overlap."
  :type 'boolean
  :group 'org-timeline)

(defcustom org-timeline-emphasize-next-block nil
  "Option to apply the face `org-timeline-next-block' to the next block happening today."
  :type 'boolean
  :group 'org-timeline)

(defcustom org-timeline-show-text-in-blocks nil
  "Option to show the text of the event in the block.

If the item has a property `TIMELINE_TEXT', use this as a title.
Otherwise, the title will be the item's headline, stripped of its todo state."
  :type 'boolean
  :group 'org-timeline)

(defcustom org-timeline-beginning-of-day-hour 5
  "When the timeline begins.

Due to the way 'org-agenda' works, if you set this to any other value than 0
\(e.g. 5), then events that happen after midnight will not appear (even though
the timeline shows the slots).
If you view the agenda in week mode, those events will not appear in any of
the week's day.

The workaround for this in day view is to use `org-timeline-keep-elapsed' that
will make the timeline show you a 24h cycle. See this variable's documentation
for more information."
  :type 'integer
  :group 'org-timeline)

(defcustom org-timeline-keep-elapsed -1
  "In day view, for today, keep only this number of fully elapsed hours.

For negative values, do not hide elapsed hours.

This can be used to see a rolling 24h cycle in the timeline.
In order to do that, set `org-timeline-beginning-of-day-hour' to 0, and set
`org-timeline-keep-elapsed' to any positive number.
Set `org-agenda-span' to 2, and open the day agenda view for today.
You will see a rolling 24h cycle, starting `org-timeline-keep-elapsed' hours ago."
  :type 'integer
  :group 'org-timeline)

(defcustom org-timeline-insert-before-text "\u275A"
  "String inserted before the block's text.

It makes consecutive blocks distinct.

The default value '\u275A' is a heavy vertical bar ❚."
  :type 'string
  :group 'org-timeline)

(defvar org-timeline-first-line-in-agenda-buffer 0
  "Line number of the first line of the timeline in the agenda buffer.")

(defvar org-timeline-height 0
  "Final height of the timeline.")

(defvar org-timeline-current-info nil
  "Current displayed info. Used to fix flickering of info.")

(defvar org-timeline-slotline (concat (mapconcat 'not (number-sequence 0 24) "|     ") "|")
  "The undecorated slotline string.")

(defvar org-timeline-next-task-today nil
  "The next task happening today.")

(cl-defstruct org-timeline-task
  id
  beg  ; in minutes
  end  ; in minutes
  offset-beg ; in points
  offset-end ; in points
  info ; copy of the agenda buffer's line
  line-in-agenda-buffer
  face
  day  ; absolute, see `calendar-absolute-from-gregorian'
  type ; "scheduled", "clocked" ...
  text
  group-name
  do-not-overlap-p ; make sure this block doesn't overlap with any other
  )


(defface org-timeline-block
  '((t (:inherit secondary-selection)))
  "Face used for printing blocks with time range information.

These are blocks that are scheduled for specific time range or
have an active timestamp with a range."
  :group 'org-timeline-faces)

(defface org-timeline-elapsed
  '((t (:inherit region)))
  "Face used for highlighting elapsed portion of the day."
  :group 'org-timeline-faces)

(defface org-timeline-clocked
  '((t (:inherit highlight)))
  "Face used for printing clocked blocks.

Clocked blocks appear in the agenda when `org-agenda-log-mode' is
activated."
  :group 'org-timeline-faces)

(defface org-timeline-overlap
  '((t (:background "dark red")))
   "Face used for printing overlapping blocks."
   :group 'org-timeline-faces)

(defface org-timeline-next-block
  '((t (:background "dark olive green")))
   "Face used for printing the next block happening today.

Used when `org-timeline-emphasize-next-block' is non-nil."
   :group 'org-timeline-faces)


(defmacro org-timeline-with-each-line (&rest body)
  "Execute BODY on each line in buffer."
  (declare (indent 0)
           (debug (body)))
  `(save-excursion
     (goto-char (point-min))
     ,@body
     (while (= (forward-line) 0)
       ,@body)))

(defun org-timeline--get-face (type)
  "Get the face with which to draw the current block, according to TYPE."
  (--if-let (org-entry-get (org-get-at-bol 'org-marker) "TIMELINE_FACE" t)
      (let ((read-face (car (read-from-string it))))
        (if (stringp read-face)
            (list :background read-face)
          read-face))
    (if (string= type "clock")
        (list 'org-timeline-clocked)
      (list 'org-timeline-block))))

(defun org-timeline--get-block-text ()
  "Get the text to print inside the current block."
  (let ((item-marker (org-get-at-bol 'org-marker)))
    (--if-let (org-entry-get item-marker "TIMELINE_TEXT" t)
        it
       (with-current-buffer (marker-buffer item-marker)
        (save-excursion
          (goto-char item-marker)
          (outline-previous-heading)
          (org-element-property :raw-value (org-element-context)))))))

(defun org-timeline--get-group-name (type)
  "Get the current block's 'group-name' according to TYPE.

The first three chars will be printed at the beginning of the block's line."
  (--if-let (org-entry-get (org-get-at-bol 'org-marker) "TIMELINE_GROUP" t)
      (if (< (length it) 3)
          (concat (substring "   " 0 (- 3 (length it))) it)
        (substring it 0 3))
    (if (and (string= type "clock") org-timeline-dedicated-clocked-line)
        "  $"
      "   ")))

(defun org-timeline--get-do-not-overlap (type)
  "Whether the current block is allowed to  overlap in the timeline according to TYPE."
  (--if-let (org-entry-get (org-get-at-bol 'org-marker) "TIMELINE_DO_NOT_OVERLAP" t)
      it
    (if (and (not (string= type "clock")) org-timeline-overlap-in-new-line)
        t
      nil)))

(defun org-timeline--overlapping-at-point (task)
  "List of points where an already drawn blocks would overlap with TASK."
  (save-excursion
    (let (overlap-points)
      (goto-char (+ (line-beginning-position) (org-timeline-task-offset-beg task)))
      (while (and (<= (point) (+ (line-beginning-position) (org-timeline-task-offset-end task)))
                 (< (point) (point-max)))
        (when (get-text-property (point) 'org-timeline-occupied)
          (push (point) overlap-points))
        (forward-char))
      overlap-points)))

(defun org-timeline--new-overlap-line-required-at-point-p (task)
  "Whether a new overlap line needs to be created to insert TASK."
  (let* ((overlapping (org-timeline--overlapping-at-point task))
         (overlapping-blocks-that-do-not-overlap
          (delq nil (mapcar (lambda (point) (get-text-property point 'org-timeline-do-not-overlap)) overlapping))))
    (and (not (eq overlapping nil))
         (or (org-timeline-task-do-not-overlap-p task)
             (not (eq overlapping-blocks-that-do-not-overlap nil))))))

(defun org-timeline--add-elapsed-face (string)
  "Add `org-timeline-elapsed' to STRING's elapsed portion.

Return new copy of STRING."
  (let* ((string-copy (copy-sequence string))
         (start-offset (* org-timeline-beginning-of-day-hour 60))
         (current-time (+ (* 60 (string-to-number (format-time-string "%H")))
                          (string-to-number (format-time-string "%M"))))
         (current-offset (/ (- current-time start-offset) 10)))
    (when (< 0 current-offset)
      (put-text-property 0 (+ 1 current-offset) 'font-lock-face (list 'org-timeline-elapsed) string-copy))
    string-copy))

(defun org-timeline--kill-info ()
  "Kill the info line."
  (save-excursion
    (goto-line org-timeline-first-line-in-agenda-buffer)
    (while (and (not (get-text-property (point) 'org-timeline-info-line))
                (eq (forward-line) 0)))
    (unless (eq (point) (point-max)) ; info line not found
      (let ((inhibit-read-only t))
        (kill-whole-line)))))

(defun org-timeline--decorate-info (info)
  "Make INFO string clickable."
  (let ((info-keymap (make-sparse-keymap)))
    (define-key info-keymap [mouse-1] 'org-agenda-goto)
    (define-key info-keymap [mouse-2] 'org-find-file-at-mouse)
    (propertize info 'keymap info-keymap
                     'help-echo "mouse-1 jump to org file"
                     'org-timeline-info-line t)))

(defun org-timeline--draw-new-info (win info)
  "Displays INFO about a hovered block.

WIN is the agenda buffer's window."
  (unless (eq info org-timeline-current-info) ; prevents flickering
    (setq org-timeline-current-info info)
    (save-window-excursion
      (save-excursion
        (select-window win) ; because one can hover blocks without being in the agenda window.
        (org-timeline--kill-info)
        (goto-line org-timeline-first-line-in-agenda-buffer)
        (forward-line (- org-timeline-height 2))
        (let ((inhibit-read-only t))
          (insert (org-timeline--decorate-info info) "\n"))))))

(defun org-timeline--move-to-task-in-agenda-buffer ()
  "Move to a block's correponding task in the agenda buffer."
  (interactive)
   (let ((line (get-text-property (point) 'org-timeline-task-line)))
     (when org-timeline-prepend
       (setq line (+ line org-timeline-height -1)))
     (goto-line line)
     (search-forward (get-text-property (point) 'time)))) ; makes point more visible to user.

(defun org-timeline--list-tasks ()
  "Build the list of tasks to display."
  (let* ((tasks nil)
         (id 0)
         (start-offset (* org-timeline-beginning-of-day-hour 60))
         (current-time (+ (* 60 (string-to-number (format-time-string "%H")))
                          (string-to-number (format-time-string "%M")))))
    (org-timeline-with-each-line
      (-when-let* ((time-of-day (org-get-at-bol 'time-of-day))
                   (marker (org-get-at-bol 'org-marker))
                   (type (org-get-at-bol 'type))
                   (duration (org-get-at-bol 'duration)))
        (when (member type (list "scheduled" "clock" "timestamp"))
          (when (and (numberp duration)
                     (< duration 0))
            (cl-incf duration 1440))
          (let* ((hour (/ time-of-day 100))
                 (minute (mod time-of-day 100))
                 (beg (+ (* hour 60) minute))
                 (end (if duration
                          (round (+ beg duration))
                        current-time)))
            (when (eq end (* 24 60)) (cl-incf end -1)) ; FIXME fixes a bug that shouldn't happen (crash when events end at midnight).
            (when (and (>= beg start-offset)
                       (or org-timeline-show-clocked
                           (not (string= type "clock"))))
              (push (make-org-timeline-task
                     :id id
                     :beg beg
                     :end end
                     :offset-beg (+ 5 (- (/ beg 10) (* 6 org-timeline-beginning-of-day-hour)))
                     :offset-end (+ 5 (- (/ end 10) (* 6 org-timeline-beginning-of-day-hour)))
                     :info (buffer-substring (line-beginning-position) (line-end-position))
                     :line-in-agenda-buffer (line-number-at-pos)
                     :face (org-timeline--get-face type)
                     :day (org-get-at-bol 'day)
                     :type type
                     :text (org-timeline--get-block-text)
                     :group-name (org-timeline--get-group-name type)
                     :do-not-overlap-p (org-timeline--get-do-not-overlap type)
                     )
                    tasks)
              (cl-incf id))))))
    ;; find the next task
    (setq org-timeline-next-task nil)
    (dolist (task tasks)
      (let* ((beg (org-timeline-task-beg task))
             (end (org-timeline-task-end task))
             (today (calendar-absolute-from-gregorian (calendar-current-date)))
             (is-today (eq today (org-timeline-task-day task)))
             (is-now (and (<= beg current-time)
                          (>= end current-time)))
             (is-after (> beg current-time))
             (is-closer-to-now (and is-after
                                    (or (eq org-timeline-next-task nil)
                                        (< beg (org-timeline-task-beg org-timeline-next-task))))))
        (when (and is-today (or is-now is-closer-to-now))
          (setq org-timeline-next-task task))))
    ;; change the next task's face
    (when org-timeline-emphasize-next-block
      (dolist (task tasks)
        (when (eq (org-timeline-task-id task) (org-timeline-task-id org-timeline-next-task))
          (setf (org-timeline-task-face task) (list 'org-timeline-next-block)))))
  (nreverse tasks)))

(defun org-timeline--goto-block-position (task)
  "Go to TASK's block's line and position cursor in line...

Return t if this task will overlap another one when inserted."
  (let* ((slotline (org-timeline--add-elapsed-face org-timeline-slotline))
         (offset-beg (org-timeline-task-offset-beg task))
         (offset-end (org-timeline-task-offset-end task))
         (day (org-timeline-task-day task))
         (group-name (org-timeline-task-group-name task))
         (do-not-overlap (org-timeline-task-do-not-overlap-p task)))
    (goto-char 1)
    (while (and (not (eq (get-text-property (point) 'org-timeline-day) day))
                (not (eq (forward-line) 1))))
    (unless (eq (get-text-property (point) 'org-timeline-day) day)
      (insert (concat "\n" ; creating the necessary lines, up to the current task's day
                      (mapconcat (lambda (line-day)
                                   (propertize (concat (calendar-day-name (mod line-day 7) t t) ; found in https://github.com/deopurkar/org-timeline
                                                       " "
                                                       slotline)
                                               'org-timeline-day line-day 'org-timeline-group-name "   "))
                                 (if-let ((last-day (get-text-property (point) 'org-timeline-day)))
                                     (number-sequence (+ 1 last-day))
                                   (list day))
                                 "\n"))))
    ;; cursor is now at beginning of the task's day's first line
    (while (and (not (string= (get-text-property (point) 'org-timeline-group-name) group-name))
                (eq (get-text-property (point) 'org-timeline-day) day))
      (forward-line))
    (unless (string= (-if-let (group-here (get-text-property (point) 'org-timeline-group-name)) group-here "   ") group-name)
      (when (not (eq (line-end-position) (point-max))) (forward-line -1))
      (goto-char (line-end-position))
      (insert "\n"
              (propertize (concat group-name " " slotline) 'org-timeline-day day 'org-timeline-group-name group-name)))
    ;; cursor is now at beginning of the task's group's first line
    (while (org-timeline--new-overlap-line-required-at-point-p task)
      (let ((decorated-slotline (propertize (concat group-name " " slotline)
                                            'org-timeline-day day
                                            'org-timeline-group-name group-name)))
        (if (eq (forward-line) 1)
            (insert (concat "\n" decorated-slotline))
          (when (not (eq (get-text-property (point) 'org-timeline-group-name) group-name)) ; reached end of group's section
            (insert (concat decorated-slotline "\n"))))))
    ;; cursor is now placed on the right line, at the right position.
    (goto-char (+ (line-beginning-position) offset-beg))))

(defun org-timeline--make-basic-block (task)
  "Make TASK's block and return it as a propertized string.

This does not take the block's context (e.g. overlap) into account."
  (let* ((blank-block (mapconcat 'not (number-sequence 1 24) "      "))
         (id (org-timeline-task-id task))
         (offset-beg (org-timeline-task-offset-beg task))
         (offset-end (org-timeline-task-offset-end task))
         (info (org-timeline-task-info task))
         (face (org-timeline-task-face task))
         (line (org-timeline-task-line-in-agenda-buffer task))
         (do-not-overlap (org-timeline-task-do-not-overlap-p task))
         (move-to-task-map '(keymap mouse-1 . org-timeline--move-to-task-in-agenda-buffer))
         (block-length (- offset-end offset-beg))
         (props (list 'font-lock-face face
                      'org-timeline-occupied t
                      'org-timeline-do-not-overlap do-not-overlap
                      'org-timeline-task-id id
                      'mouse-face '(:highlight t :box t)
                      'keymap move-to-task-map
                      'task-info info
                      'help-echo (lambda (w obj pos) ; called on block hover
                                   (org-timeline--draw-new-info w info)
                                   info)
                      'org-timeline-task-line line))
         (title (concat org-timeline-insert-before-text
                        (org-timeline-task-text task)
                        blank-block))
         (block (if org-timeline-show-text-in-blocks
                    title
                  blank-block)))
    (add-text-properties 0 block-length props block)
    (substring block 0 block-length)))

(defun org-timeline--make-and-insert-block (task)
  "Insert the TASK's block at the right position in the timeline.

Changes the block's face according to context."
  (org-timeline--goto-block-position task)
  (let ((overlapp (not (eq (org-timeline--overlapping-at-point task) nil)))
        (is-next (eq (org-timeline-task-id task) (org-timeline-task-id org-timeline-next-task)))
        (block (org-timeline--make-basic-block task)))
    (when overlapp (setq block (propertize block 'font-lock-face 'org-timeline-overlap)))
    (when is-next (setq block (propertize block 'font-lock-face 'org-timeline-next-block)))
    (unless (get-text-property (- (point) 1) 'org-timeline-overline)
      (add-text-properties 0 (length block)
                           (list 'org-timeline-overline t
                                 'font-lock-face (cons '(:overline t) (get-text-property 0 'font-lock-face block))
                                 'mouse-face (cons '(:overline t) (get-text-property 0 'mouse-face block)))
                           block))
    (setq block (substring block 0 (min (length block) (- (line-end-position) (point)))))
    (delete-char (length block))
    (insert block)))

(defun org-timeline--merge-for-24h-cycle ()
  "Kill elapsed columns in day's line according to `org-timeline-keep-elapsed'.

Move tomorrow's line to the right of today's line, to show a complete 24h cycle.
See the documentation of `org-timeline-keep-elapsed' for more information."
  ;; FIXME: quite hacky. This should probably be done directly when making the tasks list,
  ;; maybe by making all those events happen the same fake '0' day and change the offsets accordingly.
  (let* ((today (calendar-absolute-from-gregorian (calendar-current-date)))
         (current-time (+ (* 60 (string-to-number (format-time-string "%H")))
                          (string-to-number (format-time-string "%M"))))
         (elapsed-hours (- (floor (/ current-time 60)) org-timeline-beginning-of-day-hour))
         (number-of-columns-tomorrow (max 0 (- elapsed-hours org-timeline-keep-elapsed)))
         (number-of-columns-today (- 24 number-of-columns-tomorrow))
         (hourline-piece (delete-and-extract-region 6 (+ 6 (* 6 number-of-columns-tomorrow))))
         (today-line-pieces nil)
         (tomorrow-line-pieces nil)
         (blank-today-line-piece (concat "    " (substring (org-timeline--add-elapsed-face org-timeline-slotline)
                                                           0
                                                           (* 6 number-of-columns-today))))
         (blank-tomorrow-line-piece (concat "    " (substring org-timeline-slotline 0 (* 6 number-of-columns-tomorrow)))))
    (goto-char 1)
    (goto-char (line-end-position))
    (insert hourline-piece)
    ;; build (today|tomorrow)-line-pieces lists.
    (while (not (eq (forward-line) 1))
      (let ((lbeg (line-beginning-position))
            (lend (line-end-position)))
        (when (eq (get-text-property (point) 'org-timeline-day) today)
          (push (buffer-substring (- lend (* 6 number-of-columns-today) 1) lend) today-line-pieces))
        (when (eq (get-text-property (point) 'org-timeline-day) (+ today 1))
          (push (buffer-substring (+ 5 lbeg) (+ 5 lbeg (* 6 number-of-columns-tomorrow))) tomorrow-line-pieces))))
    (let ((line-diff (- (length tomorrow-line-pieces) (length today-line-pieces))))
      (dotimes (max 0 line-diff) (setq today-line-pieces (append today-line-pieces blank-today-line-piece)))
      (dotimes (max 0 (- 0 line-diff)) (setq tomorrow-line-pieces (append tomorrow-line-pieces blank-tomorrow-line-piece))))
    ;; insert them
    (goto-line 2)
    (forward-char 4)
    (dolist (piece today-line-pieces)
      (insert piece "\n" "    "))
    (goto-line 2)
    (dolist (piece tomorrow-line-pieces)
      (goto-char (line-end-position))
      (insert piece)
      (forward-line))
    (forward-line -1)
    (while (eq (forward-line) 0)
      (kill-whole-line))
    (forward-char -1)
    (kill-line)
    ;; remove elapsed face from tomorrow lines
    (goto-char 1)
    (put-text-property (+ 5  (* 6 number-of-columns-today)) (line-end-position) 'face nil)
    (while (and (eq (forward-line) 0)
                (not (eq (point) (point-max))))
      (forward-char (+ 5 (* 6 number-of-columns-today)))
      (dotimes (i (- (line-end-position) (point)))
        (when (not (get-text-property (point) 'org-timeline-occupied))
          (put-text-property (point) (+ (point) 1) 'face nil))
        (forward-char)))))

;; Some ideas for the the generation of the timeline were inspired by the
;; forked repo: https://github.com/deopurkar/org-timeline.
(defun org-timeline--generate-timeline ()
  "Generate the timeline string that will represent current agenda view."
  (let* ((hourline (concat "    "
                           (org-timeline--add-elapsed-face
                            (concat "|"
                                    (mapconcat (lambda (x) (format "%02d:00" (mod x 24)))
                                               (number-sequence org-timeline-beginning-of-day-hour (+ org-timeline-beginning-of-day-hour 23))
                                               "|")
                                    "|"))))
         (tasks (org-timeline--list-tasks))
         (today (calendar-absolute-from-gregorian (calendar-current-date)))
         (today-onlyp (eq 0 (length (delq nil (mapcar (lambda (task) (if (eq (org-timeline-task-day task) today) nil task)) tasks)))))
         (today-or-tomorrow-only-p (eq 0 (length (delq nil (mapcar (lambda (task) (if (member (org-timeline-task-day task) `(,today ,(+ today 1))) nil task)) tasks))))))
    (with-temp-buffer
      (insert hourline)
      (dolist (task tasks) (org-timeline--make-and-insert-block task))
      (when (and (> org-timeline-keep-elapsed 0)
                 today-or-tomorrow-only-p
                 (> (length tasks) 0))
        (org-timeline--merge-for-24h-cycle))
      ;; display the next block's info
      (goto-char (point-max))
      (unless (eq (length tasks) 0)
        (insert "\n"
                (if (eq org-timeline-next-task nil)
                    (propertize "  no incoming event" 'org-timeline-info-line t)
                  (org-timeline--decorate-info (org-timeline-task-info org-timeline-next-task)))))
      (buffer-string))))

(defun org-timeline-insert-timeline ()
  "Insert graphical timeline into agenda buffer."
  (unless (buffer-narrowed-p)
    (goto-char (point-min))
    (unless org-timeline-prepend
      (while (and (eq (get-text-property (line-beginning-position) 'org-agenda-type) 'agenda)
                  (not (eobp)))
        (forward-line)))
    (forward-line)
    (let ((inhibit-read-only t))
      (cursor-sensor-mode 1)
      (setq org-timeline-first-line-in-agenda-buffer (line-number-at-pos))
      (insert (propertize (concat (make-string (window-width) ?─)) 'face 'org-time-grid) "\n")
      (insert (org-timeline--generate-timeline))
      (insert (propertize (concat "\n" (make-string (window-width) ?─)) 'face 'org-time-grid 'org-timeline-end t) "\n")
      (setq org-timeline-height (- (line-number-at-pos) org-timeline-first-line-in-agenda-buffer)))
    ;; enable `font-lock-mode' in agenda view to display the "chart"
    (font-lock-mode)))

(provide 'org-timeline)
;;; org-timeline.el ends here
