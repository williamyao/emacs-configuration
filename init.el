;;;; William's .emacs file. Loads a bunch of libraries.
;;;; Fold personal code out into libraries, except for 
;;;; small miscellaneous stuff

(require 'cl) ; I wish emacs was written in Common Lisp.

;;; Paths setup.

;; (defvar user-home
;;   (cl-case system-type
;;     ((cygwin) "/home/william/")
;;     ((linux gnu/linux) "/home/william/")
;;     ((darwin) "/Users/william/")))

(defvar user-home "~/")

(defun home-path (path) (concat user-home path))

(defvar emacs-home (home-path ".emacs.d/"))

(cl-flet ((add-path (path)
            (add-to-list 'load-path (concat emacs-home path))))
  (add-path "elisp") ; personal code
  (add-path "libraries") ; third party code
  (add-path "libraries/color-theme-6.6.0/")
  (add-path "libraries/emacs-mainline/"))

(require 'cask (cl-case system-type
                 (darwin "/usr/local/Cellar/cask/0.7.2_1/cask.el")
                 (otherwise (home-path ".cask/cask.el"))))
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(load-library "general-configuration") ; mode-agnostic configuration

;;; Color theme.
(if (display-graphic-p)
    (require 'soft-morning-theme) ; soft-morning causes problems on TTY
    (progn
      (load-library "color-theme")
      (color-theme-initialize)
      (color-theme-charcoal-black)))

;;; Cask libraries.

(require 'column-marker)
(require 'company)
(require 'dash-at-point)
(require 'drag-stuff)
(require 'elpy)
(require 'expand-region)
(require 'magit)
(require 'paren-face)
(require 'paredit)
(require 'rainbow-mode)
(require 'sly)
(require 'sly-company)
(require 'smex)
(require 'vlf)
(require 'writeroom-mode)

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

(when (fboundp 'toggle-frame-fullscreen)
  (toggle-frame-fullscreen))
