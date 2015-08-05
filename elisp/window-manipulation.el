;;;; Utilities for working with windows.

;;;; Updated 2015-08-05

(defun swap-window-buffers (dist)
  "Swap the buffer in the current window with the buffer in the window
   that is DIST calls of \\[other-window] away."
  (interactive "p")
  (let ((buf1 (buffer-name)))
    (other-window dist)
    (let ((buf2 (buffer-name)))
      (switch-to-buffer buf1)
      (other-window (- dist))
      (switch-to-buffer buf2)
      (other-window dist))))
