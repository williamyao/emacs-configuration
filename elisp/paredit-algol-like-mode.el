;;;; Paredit, customized to be a little more friendly in
;;;; Algol-like languages.

(require 'paredit)

(defvar paredit-algol-like-lighter " ParAlgol")
(defvar paredit-algol-like-mode-map (copy-keymap paredit-mode-map))

;;; Copied almost verbatim from `paredit-doublequote'.
(defun paredit-algol-like-singlequote (&optional n)
  "Insert a pair of single-quotes.
With a prefix argument N, wrap the following N S-expressions in
  single-quotes, escaping intermediate characters if necessary.
If the region is active, `transient-mark-mode' is enabled, and the
  region's start and end fall in the same parenthesis depth, insert a
  pair of single-quotes around the region, again escaping intermediate
  characters if necessary.
Inside a comment, insert a literal single-quote.
At the end of a string, move past the closing single-quote.
In the middle of a string, insert a backslash-escaped single-quote.
If in a character literal, do nothing.  This prevents accidentally
  changing a what was in the character literal to become a meaningful
  delimiter unintentionally."
  (interactive "P")
  (cond ((paredit-in-string-p)
         (if (eq (point) (- (paredit-enclosing-string-end) 1))
             (forward-char)             ; Just move past the closing quote.
           ;; Don't split a \x into an escaped backslash and a string end.
           (if (paredit-in-string-escape-p) (forward-char))
           (insert ?\\ ?\' )))
        ((paredit-in-comment-p)
         (insert ?\' ))
        ((not (paredit-in-char-p))
         (paredit-insert-pair n ?\' ?\' 'paredit-forward-for-quote))))

(define-key paredit-algol-like-mode-map (kbd ";") 'self-insert-command)
(define-key paredit-algol-like-mode-map (kbd "'") 'paredit-algol-like-singlequote)
(define-key paredit-algol-like-mode-map (kbd "{") 'paredit-open-curly)
(define-key paredit-algol-like-mode-map (kbd "}") 'paredit-close-curly)

;;;###autoload
(define-minor-mode paredit-algol-like-mode
  "Paredit, modified to work well with Algol and C-like languages."
  nil
  paredit-algol-like-lighter
  paredit-algol-like-mode-map
  (when paredit-mode
    (paredit-mode -1)))

(defun paredit-disable-spacing-in-algol-like (original &rest args)
  (if paredit-algol-like-mode
      nil
    (apply original args)))

(advice-add 'paredit-space-for-delimiter-p
            :around #'paredit-disable-spacing-in-algol-like)

(provide 'paredit-algol-like-mode)
