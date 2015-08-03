(add-to-list 'load-path "~/.emacs.d/elisp/")

;;;; MELPA package repositories
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'column-marker)
(require 'timestamps)
(require 'writing)
(require 'magit)
(require 'sexp-manipulation)
(require 'company)
(require 'page-break-lines)
(require 'sly)
(require 'sly-company)

;;;; Custom keybindings
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

(global-set-key (kbd "C-x C-j") 'delete-indentation)

(global-set-key (kbd "C-x t") 'insert-todo)

(global-set-key (kbd "C-x C-h") 'magit-status)

(defun backwards-upcase-sexp ()
  (interactive)
  (upcase-sexp -1))

(global-set-key (kbd "C-x C-y") 'backwards-upcase-sexp)

(global-unset-key (kbd "RET"))
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(defun swap-window-buffers (dist)
  "Swap the buffer in the current window with the buffer in the window
   that is DIST calls of \\[other-window] away."
  (interactive "p")
  (let ((buf1 (buffer-name)))
    (other-window dist)
    (let ((buf2 (buffer-name)))
      (switch-to-buffer buf1)
      (other-window (- dist))
      (switch-to-buffer buf2)
      (other-window dist))))

(global-set-key (kbd "C-c C-d C-s") 'swap-window-buffers)

(defalias 'qrr 'query-replace-regexp)

(setq sentence-end "[\\.;:!?] ")


;;;; Remove menubar etc.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))


;;;; IDO configuration
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order
      '(".lisp" ".asd"))


;;;; ERC configuration
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(add-hook 'erc-mode-hook
	  (lambda ()
	    (erc-scrolltobottom-mode)
	    (setq erc-input-line-position -2)))


;;;; Writing text is important too
(add-hook 'text-mode-hook
	  (lambda ()
	    (visual-line-mode)))


;;;; SLY configuration
(setq inferior-lisp-program "/usr/local/bin/ccl")

(add-hook 'sly-mode-hook 'sly-company-mode)
(add-to-list 'company-backends 'sly-company)

(defun lisp-mode-customization ()
  (paredit-mode)
  (abbrev-mode)
  (paren-face-mode)
  (sly-mode)
  (hs-minor-mode)
  
  (column-marker-1 75)

  (set-face-foreground 'column-marker-1 "red")
  (set-face-background 'column-marker-1 nil)
  ;; (set-face-inverse-video 'company-tooltip-selection t)
  ;; (set-face-foreground 'parenthesis "dark slate gray")
  ;; (set-face-foreground 'font-lock-doc-face "#fdf17b")
  ;; (set-face-foreground 'font-lock-comment-face "#fdf17b")
  ;; (set-face-foreground 'font-lock-comment-delimiter-face "#fdf17b")
  ;; (set-face-foreground 'font-lock-string-face "#fdf17b")

  (local-set-key (kbd "C-w") 'paredit-backward-kill-word)
  (local-set-key (kbd "C-M-i") 'company-complete)
  (local-set-key (kbd "<C-tab>") 'hs-toggle-hiding)

  (hs-hide-all))

(add-hook 'lisp-mode-hook 'lisp-mode-customization) 


;;;; Font
(set-face-attribute 'default nil
		    :font "Anonymous Pro"
		    :height 100
		    :weight 'normal)


(setq ring-bell-function (lambda ()))
(setq show-help-function nil)

(show-paren-mode 1)


;;;; Color theme
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/")
(require 'color-theme)
(color-theme-initialize)
(color-theme-charcoal-black)

(setq truncate-partial-width-windows t)


(defun eshell-mode-hook-func ()
  (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (define-key eshell-mode-map (kbd "M-s") 'other-window-or-split))
 
(add-hook 'eshell-mode-hook 'eshell-mode-hook-func)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(newsticker-url-list
   (quote
    (("BBC News" "http://feeds.bbci.co.uk/news/world/rss.xml" nil nil nil))))
 '(newsticker-url-list-defaults
   (quote
    (("CNET News.com" "http://export.cnet.com/export/feeds/news/rss/1,11176,,00.xml"))))
 '(writeroom-width 65))

(put 'upcase-region 'disabled nil)

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.backups"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

(toggle-frame-fullscreen)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
