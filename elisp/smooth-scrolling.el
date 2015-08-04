(defvar scroll-duration-delta 7
  "Time, in milliseconds, that `scroll-smoothly-up' and
`scroll-smoothly-down' sleep per inner loop.")

(defun scroll-delta (scrolling-function lines)
  (funcall scrolling-function lines)
  (redisplay)
  (sleep-for 0 scroll-duration-delta))

(defun near-full-page () (- (window-height) next-screen-context-lines))

(defun preceeding-screen-lines ()
  (save-excursion
    (move-to-window-line 0)
    (count-screen-lines 1 (point))))

(defun remaining-screen-lines ()
  (save-excursion
    (move-to-window-line '-)
    (count-screen-lines (point))))

(defun scroll-smoothly-up (lines)
  "Scroll the current window up by LINES lines over a short duration."
  (let* ((lines-magnitude (abs lines))
	 (scrolling-function (if (plusp lines) #'scroll-up #'scroll-down)))
    (while (> lines-magnitude 3)
      (let ((scroll-amount (/ lines-magnitude 4)))
	(decf lines-magnitude scroll-amount)
	(scroll-delta scrolling-function scroll-amount)))
    (dotimes (i lines-magnitude)
      (scroll-delta scrolling-function 1))))

(defun scroll-smoothly-down (lines)
  "Scroll the current window down by LINES lines over a short duration."
  (scroll-smoothly-up (- lines)))

(defun scroll-smoothly-up-command (lines)
  "Scroll the current window up by LINES.
If LINES is omitted or NIL, scroll up by a near full page.
If LINES is -, scroll a near full page down.
A near full page is `next-screen-context-lines' less than a full page."
  (interactive "P")
  (case lines
    ((nil) (scroll-smoothly-up (min (remaining-screen-lines)
				    (near-full-page))))
    ((-) (scroll-smoothly-down (min (preceeding-screen-lines)
				    (near-full-page))))
    (otherwise (scroll-smoothly-up (min (remaining-screen-lines)
					lines)))))

(defun scroll-smoothly-down-command (lines)
  "Scroll the current window down by LINES.
If LINES is omitted or NIL, scroll down by a near full page.
If LINES is -, scroll a near full page up.
A near full page is `next-screen-context-lines' less than a full page."
  (interactive "P")
  (case lines
    ((nil) (scroll-smoothly-down (min (preceeding-screen-lines)
				      (near-full-page))))
    ((-) (scroll-smoothly-up (min (remaining-screen-lines)
				  (near-full-page))))
    (otherwise (scroll-smoothly-down (min (preceeding-screen-lines)
					  lines)))))

(provide 'smooth-scrolling)
