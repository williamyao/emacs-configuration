;;;; Extremely custom keybindings that
;;;;   a) don't depend on any packages being installed;
;;;;      any keybindings that do so should be factored out into
;;;;      the specific customization for that mode
;;;;   b) are easy to use on a *Dvorak* keyboard

(require 'cl)

(defvar dyntu-global-map (make-keymap))

(defun bind (k f) (define-key dyntu-global-map (kbd k) f))

;;; First, we set all printing keys to `self-insert-command'.
(loop for b from 32 upto 126
      do (bind (byte-to-string b) 'self-insert-command))

;;; Basic editing.
(bind "C-u" 'universal-argument)

(define-key dyntu-global-map (kbd "<backspace>") 'backward-delete-char)
(define-key dyntu-global-map (kbd "RET") 'newline-and-indent)
(define-key dyntu-global-map (kbd "C-j") 'newline)

(defun open-insert-line (p)
  (interactive "P")
  (if p
      (progn (beginning-of-line)
	     (open-line 1))
    (progn (end-of-line)
	   (open-line 1)
	   (next-line))))

(bind "C-o" 'open-insert-line)

;;; Basic movement.
(setf next-line-add-newlines nil)
(setf line-move-visual t)

(define-key dyntu-global-map (kbd "C-M-h") 'backward-char)
(bind "<left>" 'backward-char)
(define-key dyntu-global-map (kbd "C-M-t") 'next-line)
(bind "<down>" 'next-line)
(define-key dyntu-global-map (kbd "C-M-n") 'previous-line)
(bind "<up>" 'previous-line)
(define-key dyntu-global-map (kbd "C-M-s") 'forward-char)
(bind "<right>" 'forward-char)

(bind "C-a" 'move-beginning-of-line)
(bind "C-e" 'move-end-of-line)

(use-global-map dyntu-global-map)
