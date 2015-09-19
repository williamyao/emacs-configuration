;;;; Having customizes in the init file is ugly.

(custom-set-variables
 '(helm-adaptive-mode t nil (helm-adaptive))
 '(helm-autoresize-max-height 30)
 '(helm-autoresize-min-height 30)
 '(helm-autoresize-mode t)
 '(helm-default-external-file-browser "open")
 '(helm-display-header-line nil)
 '(helm-echo-input-in-header-line t)
 '(helm-mode t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-split-window-in-side-p t)
 '(powerline-display-buffer-size nil)
 '(writeroom-width 0.4))

(custom-set-faces
 '(ac-completion-face ((t (:foreground "steelblue" :underline nil :weight bold))))
 '(helm-action ((t nil)))
 '(helm-header ((t (:inherit header-line :foreground "gray39"))))
 '(helm-selection ((t (:background "steelblue" :foreground "white" :weight bold))))
 '(helm-source-header ((t (:background "#abd7f0" :foreground "black" :weight bold))))
 '(helm-visible-mark ((t (:background "steelblue" :foreground "white"))))
 '(mode-line ((t (:background "steelblue" :foreground "white"))))
 '(powerline-active1 ((t (:background "#b9c5cd" :foreground "#4c4c4c"))))
 '(powerline-active2 ((t (:background "#e1e1e1" :foreground "#4c4c4c"))))
 '(powerline-inactive1 ((t (:background "#d9d9d9" :foreground "#5f5f5f"))))
 '(powerline-inactive2 ((t (:background "#efefef" :foreground "#5f5f5f")))))
