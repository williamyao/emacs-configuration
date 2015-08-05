;;;; William's .emacs file. Loads a bunch of libraries.
;;;; Fold personal code out into libraries, except for 
;;;; small miscellaneous stuff.

(require 'cl) ; I wish emacs was written in Common Lisp.

(defvar user-home
  (case system-type
    ((cygwin linux gnu/linux) "/usr/william/")
    ((darwin) "/Users/william/")))

(defun home-path (path) (concat user-home path))

(defvar emacs-home (home-path ".emacs.d/"))

(cl-flet ((add-path (path)
         (add-to-list 'load-path (concat emacs-home path))))
  (add-path "elisp") ; personal code
  (add-path "libraries") ; third party code
  (add-path "libraries/color-theme-6.6.0"))

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;;; Third-party libraries.

(require 'color-theme)
(require 'company)
(require 'sly)
(require 'sly-company)
(require 'writeroom-mode)
(require 'magit)
(require 'column-marker)
(require 'paren-face)

;;; Personal libraries.

(load-library "sexp-manipulation")
(load-library "smooth-scrolling")
(load-library "timestamps")
(load-library "window-manipulation")
(load-library "writing")

(load-library "customize") ; I keep customizes in a separate file.

(load-library "keybinds")
(load-library "modes")

(setq ring-bell-function (lambda ()))
(setq show-help-function nil)

(setq backup-by-copying t
      backup-directory-alist `(("." . ,(home-path ".backups/")))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

(setq sentence-end "[\\.;:!?] ")

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))

(set-frame-font (font-spec :name "Anonymous Pro"
			   :size 10))

(color-theme-initialize)
(color-theme-charcoal-black)

(toggle-frame-fullscreen)

(elisp) ; A shell is just too useful, no matter what.
