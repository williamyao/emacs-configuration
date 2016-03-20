;;;; Extremely custom keybindings that
;;;;   a) don't depend on any packages being installed;
;;;;      any keybindings that do so should be factored out into
;;;;      the specific customization for that mode
;;;;   b) are easy to use on a *Dvorak* keyboard

(require 'cl)

(defvar dyntu-global-map (make-keymap))

(defun bind (k f)
  (if (fboundp f)
      (define-key dyntu-global-map (kbd k) f)
    (warn "Function `%s' is not bound. Could not bind %s."
	  f
	  k)))

;;; First, we set all printing keys to `self-insert-command'.
(loop for b from 32 upto 126
      do (bind (byte-to-string b) 'self-insert-command))
(bind "SPC" 'self-insert-command)

;;; Fundamental stuff.
(bind "M-x" 'execute-extended-command)
(bind "C-g" 'keyboard-quit)

(bind "C-x C-f" 'find-file)
(bind "C-x b" 'switch-to-buffer)

(bind "C-x C-c" 'save-buffers-kill-terminal)
(bind "C-s" 'save-buffer)

(bind "C-M-x" 'eval-defun)

;;; Editing.
(bind "C-u" 'universal-argument)

(bind "<backspace>" 'backward-delete-char)
(bind "RET" 'newline-and-indent)
(bind "C-j" 'newline)

(defun open-insert-line (p)
  (interactive "P")
  (if p
      (progn (beginning-of-line)
	     (open-line 1))
    (progn (end-of-line)
	   (open-line 1)
	   (next-line))))

(bind "C-o" 'open-insert-line)

(bind "C-w" 'backward-kill-word)
(bind "C-x C-k" 'kill-region)

(bind "C-k" 'kill-line)

(bind "C-x j" 'delete-indentation)

(bind "C-x TAB" 'indent-rigidly)
(bind "C-." 'indent-relative)

(bind "C-;" 'comment-or-uncomment-region)

(bind "C-y" 'yank)

;;; Movement.
(setf next-line-add-newlines nil)
(setf line-move-visual t)

(bind "C-M-h" 'backward-char)
(bind "<left>" 'backward-char)
(bind "C-M-t" 'next-line)
(bind "<down>" 'next-line)
(bind "C-M-n" 'previous-line)
(bind "<up>" 'previous-line)
(bind "C-M-s" 'forward-char)
(bind "<right>" 'forward-char)

(bind "C-x M-h" 'windmove-left)
(bind "C-x M-t" 'windmove-down)
(bind "C-x M-n" 'windmove-up)
(bind "C-x M-s" 'windmove-right)

(bind "C-M-," 'beginning-of-buffer)
(bind "C-M-." 'end-of-buffer)

(bind "C-v" 'scroll-up-command)
(bind "M-v" 'scroll-down-command)

(bind "C-a" 'move-beginning-of-line)
(bind "C-e" 'move-end-of-line)

(bind "C-l" 'recenter-top-bottom)

(use-global-map dyntu-global-map)
