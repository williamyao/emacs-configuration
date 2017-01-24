;;;; Dyntu customizes.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe-mode (quote (6 . 0)) nil (fringe))
 '(ledger-post-amount-alignment-at :decimal)
 '(show-paren-delay 0))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "light gray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "unknown" :family "Anonymous Pro"))))
 '(mode-line ((t (:background "#505050" :foreground "white" :box nil))))
 '(mode-line-inactive ((t (:box nil))))
 '(parenthesis ((t (:foreground "gray30"))))
 '(powerline-active1 ((t (:background "#303030" :foreground "#e0e0e0"))))
 '(powerline-active2 ((t (:background "#101010" :foreground "#cfcfcf"))))
 '(powerline-inactive1 ((t (:background "#404040" :foreground "#e0e0e0"))))
 '(powerline-inactive2 ((t (:background "#1f1f1f" :foreground "#cfcfcf")))))

(custom-theme-set-faces
 'quasi-monochrome
 '(font-lock-warning-face ((t nil)))
 '(font-lock-type-face ((t nil)))
 '(font-lock-keyword-face ((t nil)))
 '(font-lock-function-name-face ((t nil)))
 '(font-lock-builtin-face ((t (:foreground "#891f00"))))
 '(font-lock-comment-face ((t (:foreground "#bca753"))))
 '(cursor ((t (:background "deep pink"))))
 '(show-paren-match ((t (:background "gold2")))))
