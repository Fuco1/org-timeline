;; -*- lexical-binding: t -*-

(require 'org-timeline)

(defmacro org-timeline-test-helper-with-agenda (agenda start-date &rest forms)
  (declare (indent 1)
           (debug (form form body)))
  (let ((org-file (make-symbol "org-file")))
    `(progn
       (let* ((,org-file (make-temp-file "org-timeline"))
              (org-agenda-files (list ,org-file))
              (org-agenda-start-day ,start-date)
              (org-agenda-span 'day))
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
