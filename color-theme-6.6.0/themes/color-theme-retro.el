(defun color-theme-retro-green (&optional color func)
  "Plain green on black faces for those longing for the good old days."
  (interactive)
  ;; Build a list of faces without parameters
  (let ((old-faces (face-list))
	(faces)
	(face)
	(foreground (or color "green")))
    (dolist (face old-faces)
      (cond ((memq face '(bold bold-italic))
	     (add-to-list 'faces `(,face ((t (:bold t))))))
	    ((memq face '(underline italic show-paren-match))
	     (add-to-list 'faces `(,face ((t (:underline t))))))
	    ((memq face '(mode-line highlight region isearch query-replace))
	     (add-to-list 'faces `(,face ((t (:inverse-video t))))))
	    ((memq face '(mode-line-inactive show-paren-mismatch))
	     (add-to-list 'faces `(,face ((t (:box (:line-width -1)))))))
	    (t
	     (add-to-list 'faces `(,face ((t (nil))))))))
    (let ((color-theme-is-cumulative nil))
      (color-theme-install
       (append
	(list (or func 'color-theme-retro-green)
	      (list (cons 'foreground-color foreground)
		    (cons 'background-color "black")
		    (cons 'mouse-color foreground)
		    (cons 'cursor-color foreground)
		    (cons 'border-color foreground)
		    (cons 'background-mode 'light)))
	faces)))))

(defun color-theme-retro-orange ()
  "Plain orange on black faces for those longing for the good old days."
  (interactive)
  (color-theme-retro-green "#ffad33" 'color-theme-retro-orange))

(defun color-theme-retro-white ()
  (interactive)
  (color-theme-retro-green "white" 'color-theme-retro-white))
