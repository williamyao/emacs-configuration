;;;; Display relative line numbers for each window.

(require 'cl)

(defface relative-linum
  '((t . (:inherit (shadow default))))
  "Face for displaying the relative line numbers.")

(defvar-local relative-linum-overlays nil
  "Buffer-local list of relative-linum overlays.")
(defvar-local relative-linum-available-overlays nil
  "Buffer-local list of relative-linum overlays that can be used.")

(defvar relative-linum-format "%2d"
  "Format string to use to display the line numbers.")

(defun relative-linum-display-string (line-number)
  (if (zerop line-number)
      ""
    (propertize
     (format relative-linum-format line-number)
     'face 'relative-linum)))

(defun relative-linum-set-overlay-line-number (overlay line-number)
  (overlay-put overlay 'window (selected-window))
  (overlay-put overlay
               'before-string
               (propertize " "
                           'face 'relative-linum
                           'display `((margin left-margin)
                                      ,(relative-linum-display-string line-number))))
  (push overlay relative-linum-overlays))

(defun relative-linum-update ()
  (setq relative-linum-current-line (line-number-at-pos))
  (setq relative-linum-available-overlays relative-linum-overlays)
  (setq relative-linum-overlays nil)
  (dolist (window (get-buffer-window-list))
    (relative-linum-update-window window))
  (dolist (overlay relative-linum-available-overlays)
    (delete-overlay overlay))
  (setq relative-linum-available-overlays nil))

(defun relative-linum-get-overlay ()
  (move-beginning-of-line nil)
  (if relative-linum-available-overlays
      (move-overlay (pop relative-linum-available-overlays)
                    (point)
                    (point))
    (make-overlay (point) (point))))

(defun relative-linum-update-window (window)
  (with-selected-window window
    (save-excursion
      (let ((start (window-start window)))
        (cl-do ((line-number 0 (+ line-number 1)))
            ((or (bobp) (< (point) start)))
          (relative-linum-set-overlay-line-number (relative-linum-get-overlay)
                                                  line-number)
          (previous-line))))
    (save-excursion
      (let ((end (window-end window t)))
        (cl-do ((line-number 0 (+ line-number 1)))
            ((or (eobp) (> (point) end)))
          (relative-linum-set-overlay-line-number (relative-linum-get-overlay)
                                                  line-number)
          (next-line))))
    (set-window-margins window 2)))

(add-hook 'post-command-hook 'relative-linum-update)
