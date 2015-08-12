;;;; William's .emacs file. Loads a bunch of libraries.
;;;; Fold personal code out into libraries, except for 
;;;; small miscellaneous stuff

(require 'cl) ; I wish emacs was written in Common Lisp.

;;; Paths setup.

(defvar user-home
  (case system-type
    ((cygwin) "/home/william/")
    ((linux gnu/linux) "/home/william/")
    ((darwin) "/Users/william/")))

(defun home-path (path) (concat user-home path))

(defvar emacs-home (home-path ".emacs.d/"))

(cl-flet ((add-path (path)
            (add-to-list 'load-path (concat emacs-home path))))
  (add-path "elisp") ; personal code
  (add-path "libraries") ; third party code 
  (add-path "libraries/emacs-mainline/"))

(require 'cask "/usr/local/Cellar/cask/0.7.2_1/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(load-library "general-configuration") ; mode-agnostic configuration

;;; Cask libraries.

(require 'column-marker)
(require 'company)
(require 'expand-region)
(require 'magit)
(require 'paren-face)
(require 'paredit)
(require 'rainbow-mode)
(require 'sly)
(require 'sly-company)
(require 'soft-morning-theme)
(require 'writeroom-mode)
(require 'soft-morning-theme)

;;; Third-party libraries.

(require 'main-line)
(require 'clean-mode-line)

;;; Personal libraries.

(load-library "sexp-manipulation")
(load-library "smooth-scrolling")
(load-library "timestamps")
(load-library "window-manipulation")
(load-library "writing")

(load-library "keybinds")
(load-library "modes")
 
(load-library "customize") ; I keep customizes in a separate file.

(toggle-frame-fullscreen)

(eshell) ; A shell is just too useful, no matter what.
