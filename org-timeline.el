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

(defcustom org-timeline-start-hour 5
  "Starting hour of the timeline."
  :type 'integer
  :group 'org-timeline)

(defcustom org-timeline-show-clocked t
  "Option to show or hide clocked items."
  :type 'boolean
  :group 'org-timeline)

(defcustom org-timeline-clocked-in-new-line t
  "Option to render clocked items in new line."
  :type 'boolean
  :group 'org-timeline)

(defcustom org-timeline-overlap-in-new-line nil
  "Option to render overlapping blocks in new line."
  :type 'boolean
  :group 'org-timeline)

(defvar org-timeline-first-line 0
  "Computer first line of the timeline in the buffer.")

(defvar org-timeline-height 0
  "Computed height (number of lines) of the timeline.")

(defvar org-timeline-current-info nil
  "Current displayed info. Used to fix flickering of info.")


(cl-defstruct org-timeline-task
  beg  ;; offset in timeline (beginning of event)
  end  ;; offset in timeline (end of event)
  info ;; info line for the corresponding task
  line ;; line where this task is displayed in the agenda buffer
  face ;; the task block's face
  day  ;; day (gregorian list i.e `(month day year)`) when the task appears
  type ;; type of the task ("scheduled", "clocked" ...)
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


(defmacro org-timeline-with-each-line (&rest body)
  "Execute BODY on each line in buffer."
  (declare (indent 0)
           (debug (body)))
  `(save-excursion
     (goto-char (point-min))
     ,@body
     (while (= (forward-line) 0)
       ,@body)))

(defun org-timeline--get-face ()
  "Get the face with which to draw the current block."
  (--if-let (org-entry-get (org-get-at-bol 'org-marker) "TIMELINE_FACE" t)
      (let ((read-face (car (read-from-string it))))
        (if (stringp read-face)
            (list :background read-face)
          read-face))
    (cond
     ((save-excursion
        (search-forward "Clocked:" (line-end-position) t))
      'org-timeline-clocked)
     (t 'org-timeline-block))))

(defun org-timeline--add-elapsed-face (string current-offset)
  "Add `org-timeline-elapsed' to STRING's elapsed portion.

Return new copy of STRING."
  (let ((string-copy (copy-sequence string)))
    (when (< 0 current-offset)
      (put-text-property 0 current-offset 'font-lock-face 'org-timeline-elapsed string-copy))
    string-copy))

(defun org-timeline--clear-info ()
  "Clear the info line"
  (save-excursion
    (goto-line org-timeline-first-line)
    (forward-line (- org-timeline-height 1))
    (let ((inhibit-read-only t))
      (while (not (get-text-property (point) 'org-timeline-end))
        (kill-whole-line)))))

(defun org-timeline--hover-info (win txt)
  "Displays info about a hovered block"
  (unless (eq txt org-timeline-current-info)
    (setq org-timeline-current-info txt)
    (save-window-excursion
      (save-excursion
        (select-window win)
        (org-timeline--clear-info)
        (goto-line org-timeline-first-line)
        (forward-line (- org-timeline-height 1))
        (let ((inhibit-read-only t)
              (info-keymap (make-sparse-keymap)))
          (define-key info-keymap [mouse-1] 'org-agenda-goto)
          (define-key info-keymap [mouse-2] 'org-find-file-at-mouse)
          (put-text-property 0 (string-width txt) 'keymap info-keymap txt)
          (put-text-property 0 (string-width txt) 'help-echo "mouse-1 jump to org file." txt)
          (insert txt)
          (insert "\n"))))))

(defun org-timeline--move-to-task ()
  "Move to a blocks correponding task."
  (interactive
   (let ((line (get-text-property (point) 'org-timeline-task-line)))
     (when org-timeline-prepend
       (setq line (+ line org-timeline-height)))
     (goto-line line)
     (search-forward (get-text-property (point) 'time)))))

(defun org-timeline--list-tasks ()
  "Build the list of tasks to display."
  (let* ((tasks nil)
         (start-offset (* org-timeline-start-hour 60))
         (current-time (+ (* 60 (string-to-number (format-time-string "%H")))
                          (string-to-number (format-time-string "%M")))))
    (org-timeline-with-each-line
      (-when-let* ((time-of-day (org-get-at-bol 'time-of-day))
                   (marker (org-get-at-bol 'org-marker))
                   (type (org-get-at-bol 'type)))
        (when (member type (list "scheduled" "clock" "timestamp"))
          (let ((duration (org-get-at-bol 'duration))
                (info (buffer-substring (line-beginning-position) (line-end-position)))
                (line (line-number-at-pos)))
            (when (and (numberp duration)
                       (< duration 0))
              (cl-incf duration 1440))
            (let* ((hour (/ time-of-day 100))
                   (minute (mod time-of-day 100))
                   (beg (+ (* hour 60) minute))
                   (end (if duration
                            (round (+ beg duration))
                          current-time))
                   (face (org-timeline--get-face))
                   (day (org-get-at-bol 'day)))
              (when (>= beg start-offset)
                (push (make-org-timeline-task
                       :beg beg
                       :end end
                       :face face
                       :info info
                       :line line
                       :day day
                       :type type) tasks)))))))
    (nreverse tasks)))

(defun org-timeline--generate-timeline ()
  "Generate the timeline string that will represent current agenda view."
  (let* ((start-offset (* org-timeline-start-hour 60))
         (current-time (+ (* 60 (string-to-number (format-time-string "%H")))
                          (string-to-number (format-time-string "%M"))))
         (current-offset (/ (- current-time start-offset) 10))
         (slotline (org-timeline--add-elapsed-face
                    "|     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |"
                    current-offset))
         (hourline (concat "    "
                           (org-timeline--add-elapsed-face
                            (concat "|"
                                    (mapconcat (lambda (x) (format "%02d:00" (mod x 24)))
                                               (number-sequence org-timeline-start-hour (+ org-timeline-start-hour 23))
                                               "|")
                                    "|")
                            current-offset)))
         (tasks (org-timeline--list-tasks)))
    (cl-labels ((get-start-pos (current-line beg) (+ 5 (* (- current-line 1) (+ 5 (length slotline))) (/ (- beg start-offset) 10)))
                (get-end-pos (current-line end) (+ 5 (* (- current-line 1) (+ 5 (length slotline))) (/ (- end start-offset) 10))))
      (let ((current-line 1)
            (move-to-task-map (make-sparse-keymap)))
        (define-key move-to-task-map [mouse-1] 'org-timeline--move-to-task)
        (with-temp-buffer
          (insert hourline)
          (dolist (task tasks)
              (let ((beg (org-timeline-task-beg task))
                    (end (org-timeline-task-end task))
                    (info (org-timeline-task-info task))
                    (line (org-timeline-task-line task))
                    (day (org-timeline-task-day task))
                    (face (org-timeline-task-face task))
                    (type (org-timeline-task-type task)))
                (goto-char 1)
                (while (and (not (eq (get-text-property (point) 'org-timeline-line-day) day))
                            (not (eq (forward-line) 1)))) ;; while task's day line not reached in timeline
                (unless (eq (get-text-property (point) 'org-timeline-line-day) day)
                  (insert (concat "\n" ;; creating the necessary lines, up to the current task's day
                                  (mapconcat (lambda (line-day)
                                               (propertize (concat (calendar-day-name (mod line-day 7) t t) ;; by git user deopurkar
                                                                   " "
                                                                   slotline)
                                                           'org-timeline-line-day line-day))
                                             (if-let ((last-day (get-text-property (point) 'org-timeline-line-day)))
                                                 (number-sequence (+ 1 last-day))
                                               (list day))
                                             "\n"))))
                ;; cursor is now at beginning of the task's day's line
                (when (and (get-text-property (get-start-pos (line-number-at-pos) beg) 'org-timeline-occupied) ;; overlap
                           org-timeline-overlap-in-new-line
                           (not (string= type "clock"))) ;; clocks shouldn't overlap
                  (forward-line)
                  (while (and (get-text-property (get-start-pos (line-number-at-pos) beg) 'org-timeline-occupied)
                              (get-text-property (point) 'org-timeline-overlap-line))
                    (forward-line))
                  (when (eq (point) (point-max))
                    (insert "\n"))
                  (when (not (get-text-property (point) 'org-timeline-overlap-line))
                    (insert (propertize (concat "    " slotline)
                                        'org-timeline-line-day day
                                        'org-timeline-overlap-line t))
                    (print (buffer-substring-no-properties 1 (point-max)))
                    (when (eq (save-excursion (forward-line)) 0) ;; there is a clock line
                      (insert "\n"))))
                (when (and (string= type "clock")
                           org-timeline-show-clocked
                           org-timeline-clocked-in-new-line)
                  (if (get-text-property (point) 'org-timeline-clocks-open-for-day)
                      (while (not (get-text-property (point) 'org-timeline-clock-line))
                        (forward-line))
                    (progn
                      (put-text-property (point) (line-end-position) 'org-timeline-clocks-open-for-day t)
                      (forward-line)
                      (while (get-text-property (point) 'org-timeline-overlap-line) ;; go after overlap lines
                        (forward-line))
                      (when (eq (point) (point-max))
                        (insert "\n"))
                      (unless (get-text-property (point) 'org-timeline-clock-line)
                        (insert (propertize (concat "  $ " slotline)
                                            'org-timeline-line-day day
                                             'org-timeline-clock-line t))))))
                (let* ((start-pos (get-start-pos (line-number-at-pos) beg)) ;; + 4 because the week's day is shown
                       (end-pos (get-end-pos (line-number-at-pos) end))
                       (props (list 'font-lock-face (if (or (get-text-property start-pos 'org-timeline-occupied)
                                                            (get-text-property end-pos 'org-timeline-occupied))
                                                        'org-timeline-overlap
                                                      face) ;; code from git user deopurkar
                                     'org-timeline-occupied t
                                     'mouse-face 'highlight
                                     'keymap move-to-task-map
                                     'task-info info
                                     'help-echo (lambda (w obj pos)
                                                  (org-timeline--hover-info w info)
                                                  info) ;; the lambda will be called on block hover
                                     'org-timeline-task-line line)))
                  (unless (and (string= type "clock")
                               (not org-timeline-show-clocked))
                    (add-text-properties start-pos end-pos props)))
                (setq current-line 1)))
          (buffer-string))))))

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
      (setq org-timeline-first-line (line-number-at-pos))
      (insert (propertize (concat (make-string (window-width) ?─)) 'face 'org-time-grid) "\n")
      (insert (org-timeline--generate-timeline))
      (insert (propertize (concat "\n" (make-string (window-width) ?─)) 'face 'org-time-grid 'org-timeline-end t) "\n")
      (setq org-timeline-height (- (line-number-at-pos) org-timeline-first-line)))
    ;; enable `font-lock-mode' in agenda view to display the "chart"
    (font-lock-mode)))

(provide 'org-timeline)
;;; org-timeline.el ends here
