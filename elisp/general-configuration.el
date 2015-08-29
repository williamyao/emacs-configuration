;;;; Configuration that's useful for all modes.

(require 'cl)

(setq ring-bell-function (lambda ()))
(setq show-help-function nil)

(mouse-wheel-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

(setq-default sentence-end "[\\.;:!?] ")

(setq-default indent-tabs-mode nil
              tab-width 4)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))

(set-frame-font (font-spec :name "Ubuntu Mono"
                           :size 12))

(when (eql system-type 'darwin)
  (setq ns-use-srgb-colorspace nil)) ; for powerline separators

(setq truncate-lines t)

(setq backup-by-copying t
      backup-directory-alist `(("." . ,(home-path ".backups/")))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq gc-cons-threshold 20000000)
