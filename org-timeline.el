;;; org-timeline.el --- Add graphical view of agenda to agenda buffer. -*- lexical-binding: t -*-

;; Copyright (C) 2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.1.0
;; Created: 16th April 2017
;; Package-requires: ((dash "2.13.0"))
;; Keywords: calendar

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

(defface org-timeline-block
  '((t (:background "RoyalBlue")))
  "Face used for printing blocks with time range information.

These are blocks that are scheduled for specific time range or
have an active timestamp with a range."
  :group 'org-timeline-faces)

(defface org-timeline-elapsed
  '((t (:background "#555555")))
  "Face used for highlighting elapsed portion of the day."
  :group 'org-timeline-faces)

(defface org-timeline-clocked
  '((t (:background "Grey")))
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

(defun org-timeline--generate-timeline ()
  (let* ((start-offset 300)
         (current-offset (/ (- (+ (* 60 (string-to-number (format-time-string "%H")))
                                  (string-to-number (format-time-string "%M")))
                               start-offset) 10))
         (slotline (copy-sequence "|     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |"))
         (slotline (progn
                     (when (< 0 current-offset)
                       (put-text-property 0 current-offset 'font-lock-face 'org-timeline-elapsed slotline))
                     slotline))
         (timeline (concat "|05:00|06:00|07:00|08:00|09:00|10:00|11:00|12:00|13:00|14:00|15:00|16:00|17:00|18:00|19:00|20:00|21:00|22:00|23:00|00:00|01:00|02:00|03:00|04:00|"
                           "\n"
                           slotline))
         (tasks nil))
    (org-timeline-with-each-line
      (-when-let* ((time-of-day (org-get-at-bol 'time-of-day))
                   (duration (org-get-at-bol 'duration)))
        (when (< duration 0)
          (cl-incf duration 1440))
        (let* ((hour (/ time-of-day 100))
               (minute (mod time-of-day 100))
               (beg (+ (* hour 60) minute))
               (end (+ beg duration))
               (face (--if-let (org-entry-get (org-get-at-bol 'org-marker) "TIMELINE_FACE")
                         (let ((read-face (car (read-from-string it))))
                           (if (stringp read-face)
                               (list :background read-face)
                             read-face))
                       (cond
                        ((save-excursion
                           (forward-char 26)
                           (looking-at "Clocked:"))
                         'org-timeline-clocked)
                        (t 'org-timeline-block)))))
          (when (>= beg start-offset)
            (push (list beg end face) tasks)))))
    (setq tasks (nreverse tasks))
    (cl-labels ((get-start-pos (current-line beg) (+ 1 (* current-line (1+ (length slotline))) (/ (- beg start-offset) 10)))
                (get-end-pos (current-line end) (+ 1 (* current-line (1+ (length slotline))) (/ (- end start-offset) 10))))
      (let ((current-line 1))
        (with-temp-buffer
          (insert timeline)
          (-each tasks
            (-lambda ((beg end face))
              (while (get-text-property (get-start-pos current-line beg) 'occupied)
                (cl-incf current-line)
                (when (> (get-start-pos current-line beg) (point-max))
                  (save-excursion
                    (goto-char (point-max))
                    (insert "\n" slotline))))
              (let ((start-pos (get-start-pos current-line beg))
                    (end-pos (get-end-pos current-line end)))
                (put-text-property start-pos end-pos 'font-lock-face face)
                (put-text-property start-pos end-pos 'occupied t))
              (setq current-line 1)))
          (buffer-string))))))

(defun org-timeline-insert-timeline ()
  (goto-char (point-min))
  (while (eq (get-text-property (line-beginning-position) 'org-agenda-type) 'agenda)
    (forward-line))
  (forward-line)
  (let ((inhibit-read-only t))
    (insert (org-timeline--generate-timeline))
    (insert (propertize (concat "\n" (make-string (/ (window-width) 2) ?─)) 'face 'org-time-grid) "\n"))
  ;; enable `font-lock-mode' in agenda view to display the "chart"
  (font-lock-mode))

(provide 'org-timeline)
;;; org-timeline.el ends here
