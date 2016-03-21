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

(bind "C-x k" 'kill-buffer)

(bind "M-!" 'async-shell-command)
(defun shell-command-on-buffer (p)
  (interactive "P")
  (let ((replace-buffer (if p t nil)))
    (shell-command-on-region
     (point-min)
     (point-max)
     (read-string "Shell command: ")
     replace-buffer
     replace-buffer)))
(bind "C-!" 'shell-command-on-buffer)

(bind "M-:" 'eval-expression)

(bind "C-q" 'quoted-insert)

;;; Editing.
(bind "TAB" 'indent-for-tab-command)

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
(bind "C-M-k" 'kill-sexp)

(bind "C-x j" 'delete-indentation)

(bind "C-x TAB" 'indent-rigidly)
(bind "C-." 'indent-relative)

(bind "C-;" 'comment-or-uncomment-region)

(bind "C-y" 'yank)

(bind "C-x C-o" 'delete-blank-lines)

(bind "C-d" 'delete-char)
(bind "M-d" 'kill-word)

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

(bind "M-h" 'backward-word)
(bind "M-t" 'forward-sexp)
(bind "M-n" 'backward-sexp)
(bind "M-s" 'forward-word)

(bind "C-x M-h" 'windmove-left)
(bind "C-x M-t" 'windmove-down)
(bind "C-x M-n" 'windmove-up)
(bind "C-x M-s" 'windmove-right)

(bind "C-M-," 'beginning-of-buffer)
(bind "C-M-." 'end-of-buffer)

(bind "C-x o" 'other-window)
(bind "C-x 0" 'delete-window)

(bind "C-v" 'scroll-up-command)
(bind "M-v" 'scroll-down-command)

(bind "C-a" 'move-beginning-of-line)
(bind "C-e" 'move-end-of-line)

(bind "M-a" 'backward-sentence)
(bind "M-e" 'forward-sentence)

(bind "C-M-a" 'beginning-of-defun)
(bind "C-M-e" 'end-of-defun)

(bind "C-l" 'recenter-top-bottom)
(bind "M-l" 'move-to-window-line-top-bottom)

(bind "C-n" 'isearch-forward)
(bind "C-t" 'isearch-backward)

(bind "M-m" 'back-to-indentation)

(bind "C-r" 'point-to-register)
(bind "M-r" 'register-to-point)

(bind "M-g M-g" 'goto-line)

(setq set-mark-command-repeat-pop t)
(bind "C-SPC" 'set-mark-command)

(use-global-map dyntu-global-map)
