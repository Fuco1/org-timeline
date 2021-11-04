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

(defvar org-timeline-first-line 0
  "Computer first line of the timeline in the buffer.")

(defvar org-timeline-height 0
  "Computed height (number of lines) of the timeline.")

(defconst org-timeline-current-info nil
  "Current displayed info. Used to fix flickering of info.")

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
        (let ((inhibit-read-only t))
          (insert txt)
          (insert "\n"))))))

(defun org-timeline--move-to-task ()
  "Move to a blocks correponding task."
  (interactive
   (let ((line (get-text-property (point) 'org-timeline-task-line)))
     (org-timeline--clear-info)
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
                (txt (buffer-substring (line-beginning-position) (line-end-position)))
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
                   (face (org-timeline--get-face)))
              (when (>= beg start-offset)
                (push (list beg end face txt line) tasks)))))))
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
         (hourline (org-timeline--add-elapsed-face
                    (concat "|"
                            (mapconcat (lambda (x) (format "%02d:00" (mod x 24)))
                                       (number-sequence org-timeline-start-hour (+ org-timeline-start-hour 23))
                                       "|")
                            "|")
                    current-offset))
         (timeline (concat hourline "\n" slotline))
         (tasks (org-timeline--list-tasks)))
    (cl-labels ((get-start-pos (current-line beg) (+ 1 (* current-line (1+ (length slotline))) (/ (- beg start-offset) 10)))
                (get-end-pos (current-line end) (+ 1 (* current-line (1+ (length slotline))) (/ (- end start-offset) 10))))
      (let ((current-line 1)
            (move-to-task-map (make-sparse-keymap)))
        (define-key move-to-task-map [mouse-1] 'org-timeline--move-to-task)
        (with-temp-buffer
          (insert timeline)
          (-each tasks
            (-lambda ((beg end face txt line))
              (while (get-text-property (get-start-pos current-line beg) 'org-timeline-occupied)
                (cl-incf current-line)
                (when (> (get-start-pos current-line beg) (point-max))
                  (save-excursion
                    (goto-char (point-max))
                    (insert "\n" slotline))))
              (let ((start-pos (get-start-pos current-line beg))
                    (end-pos (get-end-pos current-line end))
                    (props (list 'font-lock-face face
                                 'org-timeline-occupied t
                                 'mouse-face 'highlight
                                 'keymap move-to-task-map
                                 'txt txt
                                 'help-echo (lambda (w obj pos)
                                              (org-timeline--hover-info w txt)
                                              txt) ;; the lambda will be called on block hover
                                 'org-timeline-task-line line
                                 'cursor-sensor-functions '(org-timeline--display-info))))
                (add-text-properties start-pos end-pos props))
              (setq current-line 1)))
          (buffer-string))))))

;;;###autoload
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
      (insert (org-timeline--generate-timeline))
      (insert (propertize (concat "\n" (make-string (/ (window-width) 2) ?─)) 'face 'org-time-grid 'org-timeline-end t) "\n")
      (setq org-timeline-height (- (line-number-at-pos) org-timeline-first-line)))
    ;; enable `font-lock-mode' in agenda view to display the "chart"
    (font-lock-mode)))

(provide 'org-timeline)
;;; org-timeline.el ends here
