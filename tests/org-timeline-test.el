;; -*- lexical-binding: t -*-
;;
;; - [X] basic event
;; - [ ] FIXME: basic event with active timeline but not scheduled
;; - [X] clocked item in log mode
;; - [X] overlapping item with `org-timeline-overlap-in-new-line`
;; - [X] overlapping item without `org-timeline-overlap-in-new-line`
;; - [X] two consecutive days
;; - [X] two non-consecutive days
;; - [ ]

;;; Code:

(require 'org-timeline-test-helper)

;; (prin1 (buffer-substring-no-properties (point-min) (point-max)))

(describe "org-timeline"


  (describe "when working in a single day"


    (describe "when working with non-overlapping events"


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


  (describe "when working with overlapping events"

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
                   (expect (- end start) :to-be 6))))))))
  (describe "when working with several days"


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
                   (expect (- end start) :to-be 6))))))))

;;; org-timeline-test.el ends here
