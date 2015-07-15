;;;; timestamps.el -- Insert timestamps into buffer

;;;; Features that might be required by this binary:
;;;; 
;;;;   None

(defvar *timestamp-format-string* "%Y-%m-%d"
  "Determines the structure of timestamps inserted by `insert-timestamp'.")

(defun insert-timestamp ()
  "Insert a timestamp into the current buffer.

By default, inserts timestamps in ISO 8601 format. To change this 
behavior, customize *TIMESTAMP-FORMAT-STRING*."
  (interactive)
  (insert (format-time-string *timestamp-format-string* (current-time))))

(provide 'timestamps)
