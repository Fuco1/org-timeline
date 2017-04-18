;; -*- lexical-binding: t -*-

(require 'org-timeline-test-helper)

;; (prin1 (buffer-substring-no-properties (point-min) (point-max)))

(describe "org-timeline"


  (describe "when working with non-overlapping events"


    (it "should add scheduled item to the timeline"
      (org-timeline-test-helper-with-agenda
          "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>"
        "2017-04-19"
        (org-timeline-insert-timeline)
        (let* ((start (text-property-any (point-min) (point-max) 'occupied t))
               (end (text-property-not-all start (point-max) 'occupied t)))
          (goto-char start)
          (expect (plist-get (text-properties-at (point)) 'font-lock-face) :to-be 'org-timeline-block)
          (save-excursion
            (previous-line)
            (expect (looking-at-p "|10:00") :to-be-truthy))
          (expect (- end start) :to-be 6))))

    (it "should add time-range item to the timeline"
      (org-timeline-test-helper-with-agenda
          "* TODO
  <2017-04-19 Wed 10:00-11:50>"
        "2017-04-19"
        (org-timeline-insert-timeline)
        (let* ((start (text-property-any (point-min) (point-max) 'occupied t))
               (end (text-property-not-all start (point-max) 'occupied t)))
          (goto-char start)
          (expect (plist-get (text-properties-at (point)) 'font-lock-face) :to-be 'org-timeline-block)
          (save-excursion
            (previous-line)
            (expect (looking-at-p "|10:00") :to-be-truthy))
          (expect (- end start) :to-be 11))))

    (it "should add clocked item to the timeline in log mode"
      (org-timeline-test-helper-with-agenda
          "* TODO
  :CLOCK:
  CLOCK: [2017-04-18 Tue 20:59]--[2017-04-18 Tue 21:12] =>  0:13
  :END:"
        "2017-04-18"
        (org-agenda-log-mode)
        (org-timeline-insert-timeline)
        (let* ((start (text-property-any (point-min) (point-max) 'occupied t))
               (end (text-property-not-all start (point-max) 'occupied t)))
          (goto-char start)
          (expect (plist-get (text-properties-at (point)) 'font-lock-face) :to-be 'org-timeline-clocked)
          (save-excursion
            (previous-line)
            (expect (looking-at-p "0|21:00") :to-be-truthy))
          (expect (- end start) :to-be 2)))))


  (describe "when working with overlapping events"

    (it "should add overlapping items to separate lines"
      (org-timeline-test-helper-with-agenda
          "* TODO
  SCHEDULED: <2017-04-19 Wed 10:00-11:00>
* TODO
  SCHEDULED: <2017-04-19 Wed 10:30-11:30>"
        "2017-04-19"
        (org-timeline-insert-timeline)
        ;; (prin1 (buffer-substring-no-properties (point-min) (point-max)))
        (let* ((start (text-property-any (point-min) (point-max) 'occupied t))
               (end (text-property-not-all start (point-max) 'occupied t)))
          (goto-char start)
          (expect (plist-get (text-properties-at (point)) 'font-lock-face) :to-be 'org-timeline-block)
          (save-excursion
            (previous-line)
            (expect (looking-at-p "|10:00") :to-be-truthy))
          (expect (- end start) :to-be 6)
          (goto-char end))
        (let* ((start (text-property-any (point) (point-max) 'occupied t))
               (end (text-property-not-all start (point-max) 'occupied t)))
          (goto-char start)
          (expect (plist-get (text-properties-at (point)) 'font-lock-face) :to-be 'org-timeline-block)
          (save-excursion
            (previous-line)
            (previous-line)
            (expect (looking-at-p ":00|11:00") :to-be-truthy))
          (expect (- end start) :to-be 6))))))
