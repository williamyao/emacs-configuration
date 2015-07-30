;;;; timestamps.el -- Insert timestamps into buffer

;;;; Features that might be required by this binary:
;;;; 
;;;;   None

(defvar *timestamp-format-string* "%Y-%m-%d"
  "Determines the structure of timestamps inserted by `insert-timestamp'.")

(defvar +email-address+ "williamyaoh@gmail.com"
  "Email address to use for TODO comments and other identification.")

(defun insert-timestamp ()
  "Insert a timestamp into the current buffer.

By default, inserts timestamps in ISO 8601 format. To change this 
behavior, customize *TIMESTAMP-FORMAT-STRING*."
  (interactive)
  (insert (format-time-string *timestamp-format-string* (current-time))))

(defun insert-todo ()
  "Insert a TODO comment."
  (interactive)
  (let ((beginning (point)))
    (insert "TODO ")
    (insert-timestamp)
    (insert " " +email-address+)
    (newline)
    (insert " - ")
    (comment-region beginning (point) 3)))

(provide 'timestamps)
