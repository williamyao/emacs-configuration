;;;; Various tools for manipulating s-expressions more easily.

;;;; Updated 2015-07-14

(defmacro define-sexp-operation (operation-name)
  `(defun ,(intern (format "%s%s" operation-name "-sexp")) (amount)
     (interactive "p")
     (save-excursion
       (let ((start (point)))
	 (forward-sexp amount)
	 (let ((end (point)))
	   (when (> start end)
	     (let ((temp start))
	       (setq start end
		     end temp)))
	   (,(intern (format "%s%s" operation-name "-region"))
	    start
	    end))))))

(define-sexp-operation upcase)
(define-sexp-operation downcase)
(define-sexp-operation capitalize)

(provide 'sexp-manipulation)

    

