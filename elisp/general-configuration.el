;;;; Configuration that's useful for all modes.

(require 'cl)

(setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/Library/TeX/texbin")
(add-to-list 'exec-path "/usr/local/bin")
(setenv "PYTHONPATH" "~/Library/Python/2.7/lib/python/site-packages")

(setq ring-bell-function (lambda ()))
(setq show-help-function nil)

(mouse-wheel-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(setq-default sentence-end "[\\.;:!?] ")

(setq-default read-quoted-char-radix 16)

(setq-default indent-tabs-mode nil
              tab-width 4)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))

;; (advice-add 'make-frame :after
;;             (lambda (&rest args)
;;               (set-frame-font (font-spec :name "monofur" :size 14))))

(when (eql system-type 'darwin)
  (setq ns-use-srgb-colorspace nil)) ; for powerline separators

(setq-default truncate-lines t)

(setq-default backup-by-copying t
      backup-directory-alist `(("." . ,(home-path ".backups/")))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
