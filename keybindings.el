;;;; Extremely custom keybindings that
;;;;   a) don't depend on any packages being installed;
;;;;      any keybindings that do so should be factored out into
;;;;      the specific customization for that mode
;;;;   b) are easy to use on a *Dvorak* keyboard

(require 'cl)

(defvar dyntu-global-map (make-keymap))

;;; First, we set all keys to `self-insert-command'.
(define-key dyntu-global-map [t] 'self-insert-command)

;;; Basic editing.
(define-key dyntu-global-map (kbd "<backspace>") 'backward-delete-char)
(define-key dyntu-global-map (kbd "RET") 'newline-and-indent)
(define-key dyntu-global-map (kbd "C-j") 'newline)

;;; Basic movement.
(setf next-line-add-newlines nil)
(setf line-move-visual t)

(define-key dyntu-global-map (kbd "C-h") 'backward-char)
(define-key dyntu-global-map (kbd "C-t") 'next-line)
(define-key dyntu-global-map (kbd "C-n") 'previous-line)
(define-key dyntu-global-map (kbd "C-s") 'forward-char)

(use-global-map dyntu-global-map)
