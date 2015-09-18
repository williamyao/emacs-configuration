(defun powerline-get-separator (direction)
  "Return the symbol of the current separator for DIRECTION.
DIRECTION should be either RIGHT or LEFT; a RIGHT-separator goes
on the right side of the mode line."
  (when (not (memq direction '(left right)))
    (error "Unknown separator direction '%s" direction))
  (intern (format "powerline-%s-%s"
                  (powerline-current-separator)
                  direction)))

(defvar-local powerline-which-function-format "%s()"
  "Variable to determine how to display the name of the current function.
For example, C-like languages might want to display the function name as
'function()', whereas Lisp-like languages might want it to display like
'(function)'.")

(defun powerline-lisp-setup-which-function ()
  (setq powerline-which-function-format "(%s)"))

(add-hook 'lisp-mode-hook 'powerline-lisp-setup-which-function)
(add-hook 'emacs-lisp-mode-hook 'powerline-lisp-setup-which-function)

(defpowerline powerline-buffer-status 
  (cond
   ((and buffer-file-name (buffer-modified-p)) "Modified")
   (buffer-read-only "Read-Only")
   (t "")))

(defpowerline powerline-which-function
  (concat "[fn "
          (format powerline-which-function-format
                  (format-mode-line which-func-current))
          "]"))

(defpowerline powerline-projectile
  (let ((project-name (projectile-project-name)))
    (if (not (string-equal project-name "-"))
        (format "[project %s]" project-name)
      "")))

;;;###autoload
(defun powerline-william-theme ()
  "My own custom powerline theme."
  (interactive)
  (setq minor-mode-alist (remove (assoc 'projectile-mode minor-mode-alist)
                                 minor-mode-alist))
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (mode-face (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'powerline-active1 'powerline-inactive1))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (lhs (list (powerline-raw "%e" nil 'l)
                        (when powerline-display-buffer-size
                          (powerline-buffer-size nil 'l))
                        (powerline-raw "%b" nil 'l)
                        (powerline-raw " " nil)
                        (funcall (powerline-get-separator 'left) mode-face face1)
                        (when (and (boundp which-function-mode) which-function-mode)
                          (powerline-which-function face1 'l))
                        (when (fboundp 'projectile-project-name)
                          (powerline-projectile face1 'l))
                        (powerline-raw " " face1)
                        (funcall (powerline-get-separator 'left) face1 face2)
                        (powerline-buffer-status face2 'l)))
             (rhs (list (funcall (powerline-get-separator 'right) face2 face1)
                        (powerline-raw " " face1)
                        (powerline-major-mode face1 'r)
                        (powerline-minor-modes face1 'r)
                        (funcall (powerline-get-separator 'right) face1 mode-face)
                        (powerline-raw " " nil)
                        (powerline-raw "Line %l, Col %c" nil 'r)
                        (powerline-raw "-" nil 'r)
                        (powerline-raw "%p" nil 'r))))
        (concat (powerline-render lhs)
                (powerline-fill face2 (powerline-width rhs))
                (powerline-render rhs)))))))
