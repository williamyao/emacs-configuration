;;;; Automatically scroll certain buffers to the bottom.

(defcustom scroll-to-bottom-major-modes '(comint-mode eshell-mode)
  "List of major modes in which to automatically scroll to the bottom.")

(defcustom scroll-to-bottom-minor-modes '()
  "List of minor modes in which to automatically scroll to the bottom.")

(defun maybe-scroll-to-bottom (&rest args)
  "Function that wraps around buffer/window changing functions and
automatically scrolls to the bottom in defined modes."
  (when (or (apply 'derived-mode-p scroll-to-bottom-major-modes)
            (some 'symbol-value scroll-to-bottom-minor-modes))
    (end-of-buffer)))

(advice-add 'other-window :after 'maybe-scroll-to-bottom)
(advice-add 'switch-to-buffer :after 'maybe-scroll-to-bottom)
(advice-add 'windmove-right :after 'maybe-scroll-to-bottom)
(advice-add 'windmove-left :after 'maybe-scroll-to-bottom)
(advice-add 'windmove-up :after 'maybe-scroll-to-bottom)
(advice-add 'windmove-down :after 'maybe-scroll-to-bottom)

(provide 'auto-scroll-to-end)
