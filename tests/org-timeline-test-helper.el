;; -*- lexical-binding: t -*-
;;; Code:

(require 'org-timeline)

(defmacro org-timeline-test-helper-with-agenda (agenda start-date &rest forms)
  "Run @FORMS in buffer where AGENDA's timeline was build beginning at START-DATE."
  (declare (indent 1)
           (debug (form form body)))
  (let ((org-file (make-symbol "org-file")))
    `(progn
       (let* ((,org-file (make-temp-file "org-timeline"))
              (org-agenda-files (list ,org-file))
              (org-agenda-start-day ,start-date)
              (org-agenda-span 'day)
              (org-timeline-prepend nil)
              (org-timeline-show-clocked t)
              (org-timeline-dedicated-clocked-line t)
              (org-timeline-overlap-in-new-line nil)
              (org-timeline-emphasize-next-block nil)
              (org-timeline-show-text-in-blocks nil)
              (org-timeline-beginning-of-day-hour 5)
              (org-timeline-keep-elapsed -1)
              (org-timeline-insert-before-text "")) ; not default, but better for tests
         (unwind-protect
             (progn
               (with-temp-file ,org-file
                 (insert ,agenda))
               (with-current-buffer (find-file-noselect ,org-file)
                 (org-mode)
                 (org-agenda nil "a"))
               (with-current-buffer org-agenda-buffer
                 ,@forms))
           (delete-file ,org-file))))))

(defmacro org-timeline-test-helper-with-agenda-week (agenda start-date &rest forms)
  "Run @FORMS in buffer where AGENDA's timeline was build beginning at START-DATE."
  (declare (indent 1)
           (debug (form form body)))
  (let ((org-file (make-symbol "org-file")))
    `(progn
       (let* ((,org-file (make-temp-file "org-timeline"))
              (org-agenda-files (list ,org-file))
              (org-agenda-start-day ,start-date)
              (org-agenda-span 7)
              (org-timeline-prepend nil)
              (org-timeline-show-clocked t)
              (org-timeline-dedicated-clocked-line t)
              (org-timeline-overlap-in-new-line nil)
              (org-timeline-emphasize-next-block nil)
              (org-timeline-show-text-in-blocks nil)
              (org-timeline-beginning-of-day-hour 5)
              (org-timeline-keep-elapsed -1)
              (org-timeline-insert-before-text "")) ; not default, but better for tests
         (unwind-protect
             (progn
               (with-temp-file ,org-file
                 (insert ,agenda))
               (with-current-buffer (find-file-noselect ,org-file)
                 (org-mode)
                 (org-agenda nil "a"))
               (with-current-buffer org-agenda-buffer
                 ,@forms))
           (delete-file ,org-file))))))

(defmacro org-timeline-test-helper-with-agenda-two (agenda start-date &rest forms)
  "Run @FORMS in buffer where AGENDA's timeline was build beginning at START-DATE."
  (declare (indent 1)
           (debug (form form body)))
  (let ((org-file (make-symbol "org-file")))
    `(progn
       (let* ((,org-file (make-temp-file "org-timeline"))
              (org-agenda-files (list ,org-file))
              (org-agenda-start-day ,start-date)
              (org-agenda-span 2)
              (org-timeline-prepend nil)
              (org-timeline-show-clocked t)
              (org-timeline-dedicated-clocked-line t)
              (org-timeline-overlap-in-new-line nil)
              (org-timeline-emphasize-next-block nil)
              (org-timeline-show-text-in-blocks nil)
              (org-timeline-beginning-of-day-hour 5)
              (org-timeline-keep-elapsed -1)
              (org-timeline-insert-before-text "")) ; not default, but better for tests
         (unwind-protect
             (progn
               (with-temp-file ,org-file
                 (insert ,agenda))
               (with-current-buffer (find-file-noselect ,org-file)
                 (org-mode)
                 (org-agenda nil "a"))
               (with-current-buffer org-agenda-buffer
                 ,@forms))
           (delete-file ,org-file))))))

(provide 'org-timeline-test-helper)
;;; org-timeline-test-helper.el ends here
