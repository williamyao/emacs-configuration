;;;; Personal keybindings. Load this file last.

;;;; Updated 2015-08-05

(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

(global-set-key (kbd "C-x C-j") 'delete-indentation)

(global-set-key (kbd "C-x t") 'insert-todo)

(global-set-key (kbd "C-x C-h") 'magit-status)

(global-set-key (kbd "C-x C-y") 'backwards-upcase-sexp)

(global-set-key (kbd "C-c C-d C-s") 'swap-window-buffers)

(global-set-key (kbd "C-v") 'scroll-smoothly-up-command)
(global-set-key (kbd "M-v") 'scroll-smoothly-down-command)

(global-set-key (kbd "M-h") 'er/expand-region)

(global-unset-key (kbd "RET"))
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(defalias 'qrr 'query-replace-regexp)
