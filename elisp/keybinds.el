;;;; Personal keybindings. Load this file last.

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-set-key (kbd "C-M-,") 'beginning-of-buffer)
(global-set-key (kbd "C-M-.") 'end-of-buffer)

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

(global-set-key (kbd "C-x j") 'delete-indentation)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(global-set-key (kbd "C-.") 'indent-relative)

(global-set-key (kbd "C-x t") 'insert-todo)

(global-set-key (kbd "C-x C-h") 'magit-status)

(global-set-key (kbd "C-x C-y") 'backwards-upcase-sexp)

(global-set-key (kbd "C-c C-d C-s") 'swap-window-buffers)

(global-set-key (kbd "M-h") 'er/expand-region)

(global-set-key (kbd "C-c C-d M-h") 'dash-at-point)

(global-set-key (kbd "C-M-s") 'flx-isearch-forward)
(global-set-key (kbd "C-M-r") 'flx-isearch-backward)

(global-set-key (kbd "M-,") 'back-button-local-backward)
(global-set-key (kbd "M-.") 'back-button-local-forward)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)

(global-set-key (kbd "M-i") 'aya-expand)

(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-unset-key (kbd "C-p"))
(global-unset-key (kbd "C-n"))
(global-unset-key (kbd "C-b"))
(global-unset-key (kbd "C-f"))

(global-unset-key (kbd "C-x o"))

(global-set-key (kbd "C-n") 'undo)

(defalias 'qrr 'query-replace-regexp)

(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]  
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
             [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
  (global-unset-key k))

(global-unset-key (kbd "<C-down-mouse-1>"))
