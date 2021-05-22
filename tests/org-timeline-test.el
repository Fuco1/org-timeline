;; -*- lexical-binding: t -*-
;;
;; be careful to not run those tests around midnight, as some of them might break
;;
;; - [X] basic event
;; - [X] no event
;; - [ ] FIXME: basic event with active timeline but not scheduled
;; - [X] clocked item in log mode
;; - [X] overlapping item with `org-timeline-overlap-in-new-line`
;; - [X] overlapping item without `org-timeline-overlap-in-new-line`
;; - [X] two consecutive days
;; - [X] two non-consecutive days
;; - [X] changing beginning-of-day-hour
;; - [X] custom faces (only tests for named color "firebrick")
;; - [X] overline on consecutive events
;; - [X] text directly from headline
;; - [X] custom text
;; - [X] group for one evnet
;; - [X] group and overlapping events (with overlap-in-new-line)
;; - [X] group and overlapping events (without overlap-in-new-line)
;; - [X] group event and normal overlapping events
;; - [X] group and events on consecutive days
;; - [X] group and events on non-consecutive days
;; - [X] group and overlapping events on non-consecutive days
;; - [X] two groups in one day
;; - [X] two groups in one day and overlaps
;; - [X] dedicated clocked line (only simple version, since it's equivalent to groups)
;; - [X] dedicated clocked line with overlapping events shouldn't create a new line (and shouldn't happen anyway)
;; - [X] emphasize next block with one future event (just before midnight)
;; - [X] check info line (with emphasize next block)
;; - [X] emphasize block currently happening
;; - [X] emphasize next block with two events (one just before now, one just before midnight)
;; - [X] emphasize next block with only a past event (shouldn't emphasize)
;; - [X] emphasize next block with three consecutive days (yesterday, today, tomorrow)
;; - [X] 24 hours cycle
;; - [X] 24 hours cycle uneven overlaps with more on the left
;; - [X] 24 hours cycle uneven overlaps with more on the right
;; - [X] 24 hours cycle with groups
;; - [X] 24 hours cycle with clocks
;; - [X] 24 hours cycle merge groups
;; - [X] 24 hours cycle with overlapping events and groups

;;; Code:

(require 'org-timeline-test-helper)

;; (prin1 (buffer-substring-no-properties (point-min) (point-max)))

(describe "org-timeline"


  (describe "when working with no event"


    (it "should show the timeslot"
      (org-timeline-test-helper-with-agenda
        "* this is not a scheduled event"
        "2017-04-19"
        (org-timeline-insert-timeline)
          (expect org-timeline-height :to-be 3))))


  (describe "when working with a single event"


    (it "should add scheduled item to the timeline"
      (org-timeline-test-helper-with-agenda
          "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>"
        "2017-04-19"
        (org-timeline-insert-timeline)
        (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
               (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
          (goto-char start)
          (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-block)
          (save-excursion
            (previous-line)
            (expect (looking-at-p "10:00|") :to-be-truthy))
          (expect (- end start) :to-be 6))))

  ;;    (it "should add time-range item to the timeline"
  ;;       (org-timeline-test-helper-with-agenda
  ;;           "* TODO
  ;; <2017-04-19 Wed 10:00-11:50>"
  ;;         "2017-04-19"
  ;;         (org-timeline-insert-timeline)
  ;;         (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
  ;;                (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
  ;;           (goto-char start)
  ;;           (prin1 (point))
  ;;           (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-block)
  ;;           (save-excursion
  ;;             (previous-line)
  ;;             (expect (looking-at-p "10:00|") :to-be-truthy))
  ;;           (expect (- end start) :to-be 11))))

    (describe "when working with `org-timeline-beginning-of-day-hour'"

      (it "should start the timeline at midnight"
        (org-timeline-test-helper-with-agenda
            "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>"
          "2017-04-19"
          (let ((org-timeline-beginning-of-day-hour 0))
            (org-timeline-insert-timeline)
            (goto-line org-timeline-first-line-in-agenda-buffer)
            (forward-line)
            (expect (looking-at-p "    |00:00|01:00|02:") :to-be-truthy)
            (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-block)
              (save-excursion
                (previous-line)
                (expect (looking-at-p "10:00|") :to-be-truthy))
              (expect (- end start) :to-be 6)))))

      (it "should start the timeline at 11:00 and cut an event in half"
        (org-timeline-test-helper-with-agenda
            "* TODO
  SCHEDULED: <2017-04-19 Wed 10:30-11:30>"
          "2017-04-19"
          (let ((org-timeline-beginning-of-day-hour 11))
            (org-timeline-insert-timeline)
            (goto-line org-timeline-first-line-in-agenda-buffer)
            (forward-line)
            (expect (looking-at-p "    |11:00|12:") :to-be-truthy)
            (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-block)
              (save-excursion
                (previous-line)
                (expect (looking-at-p "11:00|") :to-be-truthy))
              (expect (- end start) :to-be 3))))))

    (it "should add custom face to scheduled item in the timeline"
      (org-timeline-test-helper-with-agenda
          "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
  :PROPERTIES:
  :TIMELINE_FACE: \"firebrick\"
  :END:"
        "2017-04-19"
        (org-timeline-insert-timeline)
        (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
               (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
          (goto-char start)
          (expect (plist-get (get-text-property (point) 'font-lock-face) :background) :to-equal "firebrick")
          (save-excursion
            (previous-line)
            (expect (looking-at-p "10:00|") :to-be-truthy))
          (expect (- end start) :to-be 6))))

    (describe "when working with the block's text"

      (it "should add headline's text in the block"
        (org-timeline-test-helper-with-agenda
            "* TODO task
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>"
          "2017-04-19"
          (let ((org-timeline-show-text-in-blocks t))
            (org-timeline-insert-timeline)
            (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (expect (looking-at-p "task") :to-be-truthy)))))

      (it "should add item's custom text in the block"
        (org-timeline-test-helper-with-agenda
            "* TODO test
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
  :PROPERTIES:
  :TIMELINE_TEXT: item
  :END:"
          "2017-04-19"
          (let ((org-timeline-show-text-in-blocks t))
            (org-timeline-insert-timeline)
            (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (expect (looking-at-p "item") :to-be-truthy))))))

    (it "should add clocked item to the timeline in log mode"
      (org-timeline-test-helper-with-agenda
          "* TODO
  :LOGBOOK:
  CLOCK: [2017-04-19 Wed 20:59]--[2017-04-19 Wed 21:12] =>  0:13
  :END:"
        "2017-04-19"
        (org-agenda-log-mode)
        (let ((org-timeline-dedicated-clocked-line nil))
          (org-timeline-insert-timeline)
          (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                 (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
            (goto-char start)
            (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-clocked)
            (save-excursion
              (previous-line)
              (expect (looking-at-p "|21:00|") :to-be-truthy))
            (expect (- end start) :to-be 2))))))


  (describe "when working with several events"

    (it "should not add overlapping items to separate lines"
      (org-timeline-test-helper-with-agenda
       "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
* TODO
  SCHEDULED: <2017-04-19 Wed 10:30-11:30>
* TODO
  SCHEDULED: <2017-04-19 Wed 14:00-15:00>"
       "2017-04-19"
       (let ((org-timeline-overlap-in-new-line nil))
         (org-timeline-insert-timeline)
         (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
           (goto-char start)
           (expect (member '(:overline t) (get-text-property (point) 'font-lock-face)) :to-be-truthy)
           (goto-char (1- end))
           (expect (member '(:overline t) (get-text-property (point) 'font-lock-face)) :to-be nil)
           (goto-char end))
         (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
           (goto-char start)
           (expect (member '(:overline t) (get-text-property (point) 'font-lock-face)) :to-be-truthy)
           (goto-char end)))))

    (describe "without `org-timeline-overlap-in-new-line'"

      (it "should not add overlapping items to separate lines"
        (org-timeline-test-helper-with-agenda
         "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
* TODO
  SCHEDULED: <2017-04-19 Wed 10:30-11:30>"
         "2017-04-19"
         (let ((org-timeline-overlap-in-new-line nil))
           (org-timeline-insert-timeline)
           (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                  (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
             (goto-char start)
             (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-block)
             (save-excursion
               (previous-line)
               (expect (looking-at-p "10:00|") :to-be-truthy))
             (goto-char (1- end))
             (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-overlap)
             (expect (- end start) :to-be 9))))))

    (describe "with `org-timeline-overlap-in-new-line'"

      (it "should add overlapping items to separate lines"
        (org-timeline-test-helper-with-agenda
         "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
* TODO
  SCHEDULED: <2017-04-19 Wed 10:30-11:30>"
         "2017-04-19"
         (let ((org-timeline-overlap-in-new-line t))
           (org-timeline-insert-timeline)
           ;; (prin1 (buffer-substring-no-properties (point-min) (point-max)))
           (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                  (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
             (goto-char start)
             (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-block)
             (save-excursion
               (previous-line)
               (expect (looking-at-p "10:00|") :to-be-truthy))
             (save-excursion
               (beginning-of-line)
               (expect (looking-at-p "Wed |") :to-be-truthy))
             (expect (- end start) :to-be 6)
             (goto-char end))
           (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                  (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
             (goto-char start)
             (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-block)
             (save-excursion
               (previous-line)
               (previous-line)
               (expect (looking-at-p "00|11:00|") :to-be-truthy))
             (save-excursion
               (beginning-of-line)
               (expect (looking-at-p "    |") :to-be-truthy))
             (expect (- end start) :to-be 6))))))


    (describe "when working with consecutive days"


      (it "should add two day lines with the dayweek as a title"
        (org-timeline-test-helper-with-agenda-week
               "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
* TODO
  SCHEDULED: <2017-04-20 Thu 10:30-11:30>"
               "2017-04-19"
               (let ((org-agenda-span 2))
                 (org-timeline-insert-timeline)
                 (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                        (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                   (goto-char start)
                   (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-block)
                   (save-excursion
                     (previous-line)
                     (expect (looking-at-p "10:00|") :to-be-truthy))
                   (save-excursion
                     (beginning-of-line)
                     (expect (looking-at-p "Wed |") :to-be-truthy))
                   (expect (- end start) :to-be 6)
                   (goto-char end))
                 ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
                 (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                        (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                   (goto-char start)
                   (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-block)
                   (save-excursion
                     (previous-line)
                     (previous-line)
                     (expect (looking-at-p "00|11:00|") :to-be-truthy))
                   (save-excursion
                     (beginning-of-line)
                     (expect (looking-at-p "Thu |") :to-be-truthy))
                   (expect (- end start) :to-be 6))))))


    (describe "when working with non-consecutive days"


      (it "should add the right day lines with the dayweeks as a title"
        (org-timeline-test-helper-with-agenda-week
               "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
* TODO
  SCHEDULED: <2017-04-22 Sat 10:30-11:30>"
               "2017-04-19"
               (let ((org-agenda-span 4))
                 (org-timeline-insert-timeline)
                 (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                        (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                   (goto-char start)
                   (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-block)
                   (save-excursion
                     (previous-line)
                     (expect (looking-at-p "10:00|") :to-be-truthy))
                   (expect (- end start) :to-be 6)
                   (goto-char end))
                 ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
                 (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                        (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                   (goto-char start)
                   (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-block)
                   (save-excursion
                     (save-excursion
                       (beginning-of-line)
                       (expect (looking-at-p "Sat |") :to-be-truthy))
                     (previous-line)
                     (save-excursion
                       (beginning-of-line)
                       (expect (looking-at-p "Fri |") :to-be-truthy))
                     (previous-line)
                     (save-excursion
                       (beginning-of-line)
                       (expect (looking-at-p "Thu |") :to-be-truthy))
                     (previous-line)
                     (save-excursion
                       (beginning-of-line)
                       (expect (looking-at-p "Wed |") :to-be-truthy))
                     (previous-line)
                     (expect (looking-at-p "00|11:00|") :to-be-truthy))
                   (expect (- end start) :to-be 6))))))

    (describe "when working with a group"


      (it "should make a special line for an event's group"
        (org-timeline-test-helper-with-agenda
            "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:"
          "2017-04-19"
          (let nil
            (org-timeline-insert-timeline)
            ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
            (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "cla |") :to-be-truthy))
              (expect org-timeline-height :to-be 6)
              (expect (- end start) :to-be 6)))))

      (it "should make two lines for overlapping events in the same group"
        (org-timeline-test-helper-with-agenda
            "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:
* TODO
  SCHEDULED: <2017-04-19 Wed 10:30-12:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:"
          "2017-04-19"
          (let ((org-timeline-overlap-in-new-line t))
            (org-timeline-insert-timeline)
            ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
            (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "cla |") :to-be-truthy))
              (expect (- end start) :to-be 6)
              (goto-char end))
            (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "    |") :to-be-truthy))
              (expect (- end start) :to-be 9))
            (expect org-timeline-height :to-be 7))))

      (it "should work well with overlaps"
        (org-timeline-test-helper-with-agenda
            "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
* TODO
  SCHEDULED: <2017-04-19 Wed 10:30-11:30>
* TODO
  SCHEDULED: <2017-04-19 Wed 10:30-12:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:"
          "2017-04-19"
          (let ((org-timeline-overlap-in-new-line t))
            (org-timeline-insert-timeline)
            ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
            (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "Wed |") :to-be-truthy))
              (save-excursion
                (previous-line)
                (expect (looking-at-p "10:00|") :to-be-truthy))
              (expect (- end start) :to-be 6)
              (goto-char end))
            (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "    |") :to-be-truthy))
              (save-excursion
                (previous-line)
                (previous-line)
                (expect (looking-at-p "00|11:") :to-be-truthy))
              (expect (- end start) :to-be 6)
              (goto-char end))
            (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "cla |") :to-be-truthy))
              (save-excursion
                (previous-line)
                (previous-line)
                (previous-line)
                (expect (looking-at-p "00|11:") :to-be-truthy))
              (expect (- end start) :to-be 9))
            (expect org-timeline-height :to-be 7))))

      (it "shouldn't make two lines for overlapping events in the same group without `org-timeline-overlap-in-new-line'"
        (org-timeline-test-helper-with-agenda
            "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:
* TODO
  SCHEDULED: <2017-04-19 Wed 10:30-12:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:"
          "2017-04-19"
          (let ((org-timeline-overlap-in-new-line nil))
            (org-timeline-insert-timeline)
            ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
            (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "cla |") :to-be-truthy))
              (expect (- end start) :to-be 12))
            (expect org-timeline-height :to-be 6))))

      (it "should make two special lines for events in the same group on consecutive days"
        (org-timeline-test-helper-with-agenda-week
            "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:
* TODO
  SCHEDULED: <2017-04-20 Thu 11:30-14:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:"
          "2017-04-19"
          (let ((org-timeline-overlap-in-new-line t))
            (org-timeline-insert-timeline)
            ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
            (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "cla |") :to-be-truthy))
              (expect (- end start) :to-be 6)
              (goto-char end))
            (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "cla |") :to-be-truthy))
              (expect (- end start) :to-be 15))
            (expect org-timeline-height :to-be 8))))

      (it "should make two special lines for events in the same group on non-consecutive days"
        (org-timeline-test-helper-with-agenda-week
            "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:
* TODO
  SCHEDULED: <2017-04-22 Sat 11:30-14:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:"
          "2017-04-19"
          (let ((org-timeline-overlap-in-new-line t))
            (org-timeline-insert-timeline)
            ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
            (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "cla |") :to-be-truthy)
                (forward-line)
                (expect (looking-at-p "Thu |") :to-be-truthy)
                (forward-line)
                (expect (looking-at-p "Fri |") :to-be-truthy)
                (forward-line)
                (expect (looking-at-p "Sat |") :to-be-truthy))
              (expect (- end start) :to-be 6)
              (goto-char end))
            (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "cla |") :to-be-truthy))
              (expect (- end start) :to-be 15))
            (expect org-timeline-height :to-be 10))))

      (it "should make new lines for overlapping events in the same group in non-consecutive days"
        (org-timeline-test-helper-with-agenda-week
            "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:
* TODO
  SCHEDULED: <2017-04-19 Wed 10:30-12:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:
* TODO
  SCHEDULED: <2017-04-21 Fri 11:00-12:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:
* TODO
  SCHEDULED: <2017-04-21 Fri 11:30-14:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:"
          "2017-04-19"
          (let ((org-timeline-overlap-in-new-line t))
            (org-timeline-insert-timeline)
            ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
            (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "cla |") :to-be-truthy))
              (save-excursion
                (previous-line)
                (previous-line)
                (expect (looking-at-p "10:00|") :to-be-truthy))
              (expect (- end start) :to-be 6)
              (goto-char end))
            (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "    |") :to-be-truthy)
                (forward-line)
                (expect (looking-at-p "Thu |") :to-be-truthy)
                (forward-line)
                (expect (looking-at-p "Fri |") :to-be-truthy))
              (save-excursion
                (dotimes (n 3) (previous-line))
                (expect (looking-at-p "00|11:00|") :to-be-truthy))
              (expect (- end start) :to-be 9)
              (goto-char end))
            (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "cla |") :to-be-truthy))
              (save-excursion
                (dotimes (n 6) (previous-line))
                (expect (looking-at-p "11:00|") :to-be-truthy))
              (expect (- end start) :to-be 6)
              (goto-char end))
            (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "    |") :to-be-truthy))
              (save-excursion
                (dotimes (n 7) (previous-line))
                (expect (looking-at-p "00|12:00|") :to-be-truthy))
              (expect (- end start) :to-be 15))
            (expect org-timeline-height :to-be 11)))))


    (describe "when working with two groups"


      (it "should make special lines for events in two different groups"
        (org-timeline-test-helper-with-agenda
            "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:
* TODO
  SCHEDULED: <2017-04-19 Wed 10:30-12:00>
  :PROPERTIES:
  :TIMELINE_GROUP: chores
  :END:"
          "2017-04-19"
          (let ((org-timeline-overlap-in-new-line t))
            (org-timeline-insert-timeline)
            ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
            (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "cla |") :to-be-truthy))
              (expect (- end start) :to-be 6)
              (goto-char end))
            (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "cho |") :to-be-truthy))
              (expect (- end start) :to-be 9))
            (expect org-timeline-height :to-be 7))))

      (it "should make a new line for overlapping events in two different groups"
        (org-timeline-test-helper-with-agenda-week
            "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:
* TODO
  SCHEDULED: <2017-04-19 Wed 10:30-12:00>
  :PROPERTIES:
  :TIMELINE_GROUP: classes
  :END:
* TODO
  SCHEDULED: <2017-04-19 Wed 11:00-12:00>
  :PROPERTIES:
  :TIMELINE_GROUP: chores
  :END:
* TODO
  SCHEDULED: <2017-04-19 Wed 11:30-14:00>
  :PROPERTIES:
  :TIMELINE_GROUP: chores
  :END:"
          "2017-04-19"
          (let ((org-timeline-overlap-in-new-line t))
            (org-timeline-insert-timeline)
            ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
            (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "cla |") :to-be-truthy))
              (save-excursion
                (previous-line)
                (previous-line)
                (expect (looking-at-p "10:00|") :to-be-truthy))
              (expect (- end start) :to-be 6)
              (goto-char end))
            (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "    |") :to-be-truthy))
              (save-excursion
                (dotimes (n 3) (previous-line))
                (expect (looking-at-p "00|11:00|") :to-be-truthy))
              (expect (- end start) :to-be 9)
              (goto-char end))
            (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "cho |") :to-be-truthy))
              (save-excursion
                (dotimes (n 4) (previous-line))
                (expect (looking-at-p "11:00|") :to-be-truthy))
              (expect (- end start) :to-be 6)
              (goto-char end))
            (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                   (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
              (goto-char start)
              (save-excursion
                (beginning-of-line)
                (expect (looking-at-p "    |") :to-be-truthy))
              (save-excursion
                (dotimes (n 5) (previous-line))
                (expect (looking-at-p "00|12:00|") :to-be-truthy))
              (expect (- end start) :to-be 15))
            (expect org-timeline-height :to-be 9)))))


    (describe "with `org-timeline-dedicated-clocked-line'"


      (it "should make a special line for clocked items"
        (org-timeline-test-helper-with-agenda
          "* TODO
  :LOGBOOK:
  CLOCK: [2017-04-19 Wed 20:59]--[2017-04-19 Wed 21:12] =>  0:13
  :END:"
        "2017-04-19"
        (org-agenda-log-mode)
        (let ((org-timeline-dedicated-clocked-line t))
          (org-timeline-insert-timeline)
          ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
          (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                 (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
            (goto-char start)
            (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-clocked)
            (save-excursion
              (beginning-of-line)
              (expect (looking-at-p "  $ ") :to-be-truthy))
            (save-excursion
              (previous-line)
              (previous-line)
              (expect (looking-at-p "|21:00|") :to-be-truthy))
            (expect (- end start) :to-be 2))
          (expect org-timeline-height :to-be 6))))

      (it "shouldn't make a new line with overlapping clocks (that shouldn't happen)"
        (org-timeline-test-helper-with-agenda
          "* TODO
  :LOGBOOK:
  CLOCK: [2017-04-19 Wed 21:01]--[2017-04-19 Wed 21:42] =>  0:41
  CLOCK: [2017-04-19 Wed 21:24]--[2017-04-19 Wed 22:59] =>  1:25
  :END:"
        "2017-04-19"
        (org-agenda-log-mode)
        (let ((org-timeline-dedicated-clocked-line t)
              (org-timeline-overlap-in-new-line t))
          (org-timeline-insert-timeline)
          ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
          (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                 (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
            (goto-char start)
            (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-clocked)
            (save-excursion
              (beginning-of-line)
              (expect (looking-at-p "  $ ") :to-be-truthy))
            (save-excursion
              (previous-line)
              (previous-line)
              (expect (looking-at-p "21:00|") :to-be-truthy))
            (expect (- end start) :to-be 11))
          (expect org-timeline-height :to-be 6)))))


    (describe "when working with `org-timeline-emphasize-next-block'"


      (it "should emphasize the next block"
        (let* ((today (calendar-current-date))
               (day (format "%02d" (calendar-extract-day today)))
               (month (format "%02d" (calendar-extract-month today)))
               (year (number-to-string (calendar-extract-year today))))
          (org-timeline-test-helper-with-agenda
              (concat "* TODO
  SCHEDULED: <" year "-" month "-" day " Wed 23:40-23:59>")
            (concat year "-" month "-" day)
            (let ((org-timeline-emphasize-next-block t))
              (org-timeline-insert-timeline)
              ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
              (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-next-block)
                (expect (- end start) :to-be 1))))))

      (it "should show the next block's info line"
        (let* ((today (calendar-current-date))
               (day (format "%02d" (calendar-extract-day today)))
               (month (format "%02d" (calendar-extract-month today)))
               (year (number-to-string (calendar-extract-year today))))
          (org-timeline-test-helper-with-agenda
              (concat "* TODO task info
  SCHEDULED: <" year "-" month "-" day " Wed 23:40-23:59>")
            (concat year "-" month "-" day)
            (let ((org-timeline-emphasize-next-block t))
              (org-timeline-insert-timeline)
              ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
              (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (forward-line)
                (goto-char (- (line-end-position) 9))
                (expect (looking-at-p "task info") :to-be-truthy)
                (expect (- end start) :to-be 1))))))

      (it "should emphasize a currently happening \"next block\""
        (let* ((today (calendar-current-date))
               (day (format "%02d" (calendar-extract-day today)))
               (month (format "%02d" (calendar-extract-month today)))
               (year (number-to-string (calendar-extract-year today)))
               (now (decode-time (current-time)))
               (hour (decoded-time-hour now))
               (beg (format "%02d" (1- hour)))
               (end (format "%02d" (1+ hour))))
          (org-timeline-test-helper-with-agenda
              (concat "* TODO
  SCHEDULED: <" year "-" month "-" day " Wed " beg ":00-" end ":00>")
            (concat year "-" month "-" day)
            (let ((org-timeline-emphasize-next-block t)
                  (org-timeline-beginning-of-day-hour 0))
              (org-timeline-insert-timeline)
              ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
              (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-next-block))))))

      (it "shouldn't emphasize the previous block"
        (let* ((today (calendar-current-date))
               (day (format "%02d" (calendar-extract-day today)))
               (month (format "%02d" (calendar-extract-month today)))
               (year (number-to-string (calendar-extract-year today))))
          (org-timeline-test-helper-with-agenda
              (concat "* TODO
  SCHEDULED: <" year "-" month "-" day " Wed 00:01-00:20>")
            (concat year "-" month "-" day)
            (let ((org-timeline-emphasize-next-block t)
                  (org-timeline-beginning-of-day-hour 0))
              (org-timeline-insert-timeline)
              ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
              (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (expect (get-text-property (point) 'font-lock-face) :not :to-contain 'org-timeline-next-block)
                (expect (- end start) :to-be 2))))))

      (it "shouldn't emphasize the previous block, only the next"
        (let* ((today (calendar-current-date))
               (day (format "%02d" (calendar-extract-day today)))
               (month (format "%02d" (calendar-extract-month today)))
               (year (number-to-string (calendar-extract-year today))))
          (org-timeline-test-helper-with-agenda
              (concat "* TODO
  SCHEDULED: <" year "-" month "-" day " Wed 00:01-00:20>
* TODO
  SCHEDULED: <" year "-" month "-" day " Wed 23:40-23:59>")
            (concat year "-" month "-" day)
            (let ((org-timeline-emphasize-next-block t)
                  (org-timeline-beginning-of-day-hour 0))
              (org-timeline-insert-timeline)
              ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
              (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (expect (get-text-property (point) 'font-lock-face) :not :to-contain 'org-timeline-next-block)
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-next-block))))))

      (it "should only emphasize today, not yesterday or tomorrow"
        (let* ((today (calendar-current-date))
               (day (format "%02d" (calendar-extract-day today)))
               (month (format "%02d" (calendar-extract-month today)))
               (year (number-to-string (calendar-extract-year today)))
               (tomorrow (decoded-time-add (decode-time (current-time)) (make-decoded-time :day 1)))
               (day-t (format "%02d" (decoded-time-day tomorrow)))
               (month-t (format "%02d" (decoded-time-month tomorrow)))
               (year-t (number-to-string (decoded-time-year tomorrow)))
               (yesterday (decoded-time-add (decode-time (current-time)) (make-decoded-time :day -1)))
               (day-y (format "%02d" (decoded-time-day yesterday)))
               (month-y (format "%02d" (decoded-time-month yesterday)))
               (year-y (number-to-string (decoded-time-year yesterday))))
          (org-timeline-test-helper-with-agenda-week
              (concat "* TODO
  SCHEDULED: <" year "-" month "-" day-y " 10:00-23:59>
* TODO
  SCHEDULED: <" year "-" month "-" day " 23:40-23:59>
* TODO
  SCHEDULED: <" year "-" month "-" day-t " 10:00-23:59>")
            (concat year "-" month "-" day-y)
            (let ((org-timeline-emphasize-next-block t)
                  (org-timeline-beginning-of-day-hour 0))
              (org-timeline-insert-timeline)
              ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
              (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (expect (get-text-property (point) 'font-lock-face) :not :to-contain 'org-timeline-next-block)
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (expect (get-text-property (point) 'font-lock-face) :to-contain 'org-timeline-next-block)
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (expect (get-text-property (point) 'font-lock-face) :not :to-contain 'org-timeline-next-block)))))))


    (describe "when working with the 24h cycle"))


      (it "should make two consecutive days into a 24h hours cycle"
        (let* ((today (calendar-current-date))
               (day (format "%02d" (calendar-extract-day today)))
               (month (format "%02d" (calendar-extract-month today)))
               (year (number-to-string (calendar-extract-year today)))
               (weekday (calendar-day-name (mod (calendar-absolute-from-gregorian today) 7) t t))
               (now (decode-time (current-time)))
               (tomorrow (decoded-time-add now (make-decoded-time :day 1)))
               (day-t (format "%02d" (decoded-time-day tomorrow)))
               (month-t (format "%02d" (decoded-time-month tomorrow)))
               (year-t (number-to-string (decoded-time-year tomorrow)))
               (hour (decoded-time-hour now))
               (beg (format "%02d" hour))
               (end (format "%02d" (1+ hour))))
          (org-timeline-test-helper-with-agenda-two
              (concat "* TODO
  SCHEDULED: <" year "-" month "-" day " " beg ":00-" end ":00>
* TODO
  SCHEDULED: <" year-t "-" month-t "-" day-t " 00:00-00:30>
* TODO
  SCHEDULED: <" year-t "-" month-t "-" day-t " 23:30-23:59>")
            (concat year "-" month "-" day)
            (let ((org-timeline-beginning-of-day-hour 0)
                  (org-timeline-keep-elapsed 0))
              (org-timeline-insert-timeline)
              ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
              (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (expect (looking-at-p (concat beg ":00|")) :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat weekday " |")) :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (goto-char end)
                  (expect (get-text-property (point) 'face) :to-be nil))
                (save-excursion
                  (previous-line)
                  (expect (looking-at-p "00:00|") :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat weekday " |")) :to-be-truthy))
                (goto-char end))
              (expect (text-property-any (point) (point-max) 'org-timeline-occupied t) :to-be nil)))))

      (it "should balance an uneven number of overlaps (more on left)"
        (let* ((today (calendar-current-date))
               (day (format "%02d" (calendar-extract-day today)))
               (month (format "%02d" (calendar-extract-month today)))
               (year (number-to-string (calendar-extract-year today)))
               (weekday (calendar-day-name (mod (calendar-absolute-from-gregorian today) 7) t t))
               (now (decode-time (current-time)))
               (tomorrow (decoded-time-add now (make-decoded-time :day 1)))
               (day-t (format "%02d" (decoded-time-day tomorrow)))
               (month-t (format "%02d" (decoded-time-month tomorrow)))
               (year-t (number-to-string (decoded-time-year tomorrow)))
               (hour (decoded-time-hour now))
               (beg (format "%02d" hour))
               (end (format "%02d" (1+ hour))))
          (org-timeline-test-helper-with-agenda-two
              (concat "* TODO
  SCHEDULED: <" year "-" month "-" day " " beg ":00-" end ":00>
* TODO
  SCHEDULED: <" year "-" month "-" day " " beg ":00-" end ":10>
* TODO
  SCHEDULED: <" year-t "-" month-t "-" day-t " 00:00-00:30>")
            (concat year "-" month "-" day)
            (let ((org-timeline-beginning-of-day-hour 0)
                  (org-timeline-overlap-in-new-line t)
                  (org-timeline-keep-elapsed 0))
              (org-timeline-insert-timeline)
              ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
              (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (expect (looking-at-p (concat beg ":00|")) :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat weekday " |")) :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (expect (looking-at-p "00:00|") :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat weekday " |")) :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (previous-line)
                  (expect (looking-at-p (concat beg ":00|")) :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p "    |") :to-be-truthy))
                (save-excursion
                  (end-of-line)
                  (previous-line)
                  (previous-line)
                  (forward-char -6)
                  (expect (looking-at-p (format "%02d:00|" (1- hour))) :to-be-truthy)))))))

      (it "should balance an uneven number of overlaps (more on right)"
        (let* ((today (calendar-current-date))
               (day (format "%02d" (calendar-extract-day today)))
               (month (format "%02d" (calendar-extract-month today)))
               (year (number-to-string (calendar-extract-year today)))
               (weekday (calendar-day-name (mod (calendar-absolute-from-gregorian today) 7) t t))
               (now (decode-time (current-time)))
               (tomorrow (decoded-time-add now (make-decoded-time :day 1)))
               (day-t (format "%02d" (decoded-time-day tomorrow)))
               (month-t (format "%02d" (decoded-time-month tomorrow)))
               (year-t (number-to-string (decoded-time-year tomorrow)))
               (hour (decoded-time-hour now))
               (beg (format "%02d" hour))
               (end (format "%02d" (1+ hour))))
          (org-timeline-test-helper-with-agenda-two
              (concat "* TODO item1
  SCHEDULED: <" year "-" month "-" day " " beg ":00-" end ":00>
* TODO item2
  SCHEDULED: <" year-t "-" month-t "-" day-t " 00:00-00:30>
* TODO item3
  SCHEDULED: <" year-t "-" month-t "-" day-t " 00:00-00:40>")
            (concat year "-" month "-" day)
            ;; The test breaks when the line marked with '*' below is removed,
            ;; but this is a buttercup specific bug.
            ;; The test passes perfectly well when ran manually, even
            ;; when running in `emacs -nw'.
            (let ((org-timeline-beginning-of-day-hour 0)
                  (org-timeline-overlap-in-new-line t)
                  (org-timeline-show-text-in-blocks t) ; *
                  (org-timeline-keep-elapsed 0))
              (org-timeline-insert-timeline)
              ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
              (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (expect (looking-at-p (concat beg ":00|")) :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat weekday " |")) :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (expect (looking-at-p "00:00|") :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat weekday " |")) :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (previous-line)
                  (expect (looking-at-p "00:00|") :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p "    |") :to-be-truthy))
                (save-excursion
                  (end-of-line)
                  (previous-line)
                  (previous-line)
                  (forward-char -6)
                  (expect (looking-at-p (format "%02d:00|" (1- hour))) :to-be-truthy)))))))

      (it "should work well with groups"
        (let* ((today (calendar-current-date))
               (day (format "%02d" (calendar-extract-day today)))
               (month (format "%02d" (calendar-extract-month today)))
               (year (number-to-string (calendar-extract-year today)))
               (weekday (calendar-day-name (mod (calendar-absolute-from-gregorian today) 7) t t))
               (now (decode-time (current-time)))
               (tomorrow (decoded-time-add now (make-decoded-time :day 1)))
               (day-t (format "%02d" (decoded-time-day tomorrow)))
               (month-t (format "%02d" (decoded-time-month tomorrow)))
               (year-t (number-to-string (decoded-time-year tomorrow)))
               (hour (decoded-time-hour now))
               (beg (format "%02d" hour))
               (end (format "%02d" (1+ hour))))
          (org-timeline-test-helper-with-agenda-two
              (concat "* TODO
  SCHEDULED: <" year "-" month "-" day " " beg ":00-" end ":00>
* TODO
  SCHEDULED: <" year "-" month "-" day " " beg ":00-" end ":00>
  :PROPERTIES:
  :TIMELINE_GROUP: left
  :END:
* TODO
  SCHEDULED: <" year-t "-" month-t "-" day-t " 00:00-00:30>
* TODO
  SCHEDULED: <" year-t "-" month-t "-" day-t " 00:00-00:30>
  :PROPERTIES:
  :TIMELINE_GROUP: right
  :END:")
            (concat year "-" month "-" day)
            (let ((org-timeline-beginning-of-day-hour 0)
                  (org-timeline-overlap-in-new-line t)
                  (org-timeline-keep-elapsed 0))
              (org-timeline-insert-timeline)
              ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
              (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (expect (looking-at-p (concat beg ":00|")) :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat weekday " |")) :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (expect (looking-at-p "00:00|") :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat weekday " |")) :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (previous-line)
                  (expect (looking-at-p (concat beg ":00|")) :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat "lef |")) :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (previous-line)
                  (previous-line)
                  (expect (looking-at-p "00:00|") :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p "rig |") :to-be-truthy)))))))

      (it "should work well with clocks"
        (let* ((today (calendar-current-date))
               (day (format "%02d" (calendar-extract-day today)))
               (month (format "%02d" (calendar-extract-month today)))
               (year (number-to-string (calendar-extract-year today)))
               (weekday (calendar-day-name (mod (calendar-absolute-from-gregorian today) 7) t t))
               (now (decode-time (current-time)))
               (tomorrow (decoded-time-add now (make-decoded-time :day 1)))
               (day-t (format "%02d" (decoded-time-day tomorrow)))
               (month-t (format "%02d" (decoded-time-month tomorrow)))
               (year-t (number-to-string (decoded-time-year tomorrow)))
               (hour (decoded-time-hour now))
               (beg (format "%02d" hour))
               (end (format "%02d" (1+ hour))))
          (org-timeline-test-helper-with-agenda-two
              (concat "* TODO
  SCHEDULED: <" year "-" month "-" day " " beg ":00-" end ":00>
* TODO
  :LOGBOOK:
  CLOCK: [" year "-" month "-" day " " beg ":10]--[" year "-" month "-" day " " end ":10] => 1:00
  :END:
* TODO
  SCHEDULED: <" year-t "-" month-t "-" day-t " 00:00-00:30>
* TODO
  :LOGBOOK:
  CLOCK: [" year-t "-" month-t "-" day-t " 00:10]--[" year-t "-" month-t "-" day-t "01:10] => 1:00
  :END:")
            (concat year "-" month "-" day)
            (let ((org-timeline-beginning-of-day-hour 0)
                  (org-timeline-overlap-in-new-line t)
                  (org-timeline-keep-elapsed 0))
              (org-agenda-log-mode)
              (org-timeline-insert-timeline)
              ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
              (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (expect (looking-at-p (concat beg ":00|")) :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat weekday " |")) :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (expect (looking-at-p "00:00|") :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat weekday " |")) :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (previous-line)
                  (forward-char -1)
                  (expect (looking-at-p (concat beg ":00|")) :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat "  $ |")) :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (previous-line)
                  (forward-char -1)
                  (expect (looking-at-p "00:00|") :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p "  $ |") :to-be-truthy)))))))

      (it "should merge groups on left and right"
        (let* ((today (calendar-current-date))
               (day (format "%02d" (calendar-extract-day today)))
               (month (format "%02d" (calendar-extract-month today)))
               (year (number-to-string (calendar-extract-year today)))
               (weekday (calendar-day-name (mod (calendar-absolute-from-gregorian today) 7) t t))
               (now (decode-time (current-time)))
               (tomorrow (decoded-time-add now (make-decoded-time :day 1)))
               (day-t (format "%02d" (decoded-time-day tomorrow)))
               (month-t (format "%02d" (decoded-time-month tomorrow)))
               (year-t (number-to-string (decoded-time-year tomorrow)))
               (hour (decoded-time-hour now))
               (beg (format "%02d" hour))
               (end (format "%02d" (1+ hour))))
          (org-timeline-test-helper-with-agenda-two
              (concat "* TODO lr-l
  SCHEDULED: <" year "-" month "-" day " " beg ":00-" end ":00>
  :PROPERTIES:
  :TIMELINE_GROUP: lr
  :END:
* TODO l
  SCHEDULED: <" year "-" month "-" day " " beg ":00-" end ":00>
  :PROPERTIES:
  :TIMELINE_GROUP: left
  :END:
* TODO lr-r
  SCHEDULED: <" year-t "-" month-t "-" day-t " 00:00-00:30>
  :PROPERTIES:
  :TIMELINE_GROUP: lr
  :END:
* TODO l
  SCHEDULED: <" year-t "-" month-t "-" day-t " 00:00-00:30>
  :PROPERTIES:
  :TIMELINE_GROUP: right
  :END:")
            (concat year "-" month "-" day)
            (let ((org-timeline-beginning-of-day-hour 0)
                  (org-timeline-overlap-in-new-line t)
                  (org-timeline-keep-elapsed 0))
              (org-timeline-insert-timeline)
              ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
              (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (previous-line)
                  (expect (looking-at-p (concat beg ":00|")) :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat " lr |")) :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (previous-line)
                  (expect (looking-at-p "00:00|") :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat " lr |")) :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (dotimes (n 3) (previous-line))
                  (expect (looking-at-p (concat beg ":00|")) :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat "lef |")) :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (dotimes (n 4) (previous-line))
                  (expect (looking-at-p "00:00|") :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p "rig |") :to-be-truthy)))))))

      (it "should work well with overlapping events and groups"
        (let* ((today (calendar-current-date))
               (day (format "%02d" (calendar-extract-day today)))
               (month (format "%02d" (calendar-extract-month today)))
               (year (number-to-string (calendar-extract-year today)))
               (weekday (calendar-day-name (mod (calendar-absolute-from-gregorian today) 7) t t))
               (now (decode-time (current-time)))
               (tomorrow (decoded-time-add now (make-decoded-time :day 1)))
               (day-t (format "%02d" (decoded-time-day tomorrow)))
               (month-t (format "%02d" (decoded-time-month tomorrow)))
               (year-t (number-to-string (decoded-time-year tomorrow)))
               (hour (decoded-time-hour now))
               (beg (format "%02d" hour))
               (end (format "%02d" (1+ hour))))
          (org-timeline-test-helper-with-agenda-two
              (concat "* TODO d1
  SCHEDULED: <" year "-" month "-" day " " beg ":00-" end ":00>
* TODO d1-o
  SCHEDULED: <" year "-" month "-" day " " beg ":10-" end ":10>
* TODO d1-g
  SCHEDULED: <" year "-" month "-" day " " beg ":10-" end ":10>
  :PROPERTIES:
  :TIMELINE_GROUP: left
  :END:
* TODO d2
  SCHEDULED: <" year-t "-" month-t "-" day-t " 00:00-00:30>
* TODO d2-o
  SCHEDULED: <" year-t "-" month-t "-" day-t " 00:10-00:40>
* TODO d2-g
  SCHEDULED: <" year-t "-" month-t "-" day-t " 00:00-00:30>
  :PROPERTIES:
  :TIMELINE_GROUP: right
  :END:")
            (concat year "-" month "-" day)
            (let ((org-timeline-beginning-of-day-hour 0)
                  (org-timeline-overlap-in-new-line t)
                  (org-timeline-show-text-in-blocks t)
                  (org-timeline-keep-elapsed 0))
              (org-timeline-insert-timeline)
              ;; (display-warning 'buttercup (format "%s" (buffer-substring (point-min) (point-max))))
              (let* ((start (text-property-any (point-min) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (expect (looking-at-p (concat beg ":00|")) :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat weekday " |")) :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (expect (looking-at-p "00:00|") :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p (concat weekday " |")) :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (previous-line)
                  (forward-char -1)
                  (expect (looking-at-p (concat beg ":00")):to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p "    |") :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (previous-line)
                  (previous-line)
                  (forward-char -1)
                  (expect (looking-at-p "00:00|") :to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p "    |") :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (dotimes (n 3) (previous-line))
                  (forward-char -1)
                  (expect (looking-at-p (concat beg ":00")):to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p "lef |") :to-be-truthy))
                (goto-char end))
              (let* ((start (text-property-any (point) (point-max) 'org-timeline-occupied t))
                     (end (text-property-not-all start (point-max) 'org-timeline-occupied t)))
                (goto-char start)
                (save-excursion
                  (dotimes (n 4) (previous-line))
                  (expect (looking-at-p "00:00|"):to-be-truthy))
                (save-excursion
                  (beginning-of-line)
                  (expect (looking-at-p "rig |") :to-be-truthy))
                (goto-char end)))))))

;;; org-timeline-test.el ends here
