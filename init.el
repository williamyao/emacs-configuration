;;;; William's .emacs file. Loads a bunch of libraries.
;;;; Fold personal code out into libraries, except for 
;;;; small miscellaneous stuff

(require 'cl) ; I wish emacs was written in Common Lisp.

;;; Paths setup.

(defvar user-home "~/")

(defun home-path (path) (concat user-home path))

(defmacro when-graphical (&rest body)
  `(when (display-graphic-p)
     ,@body))

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
(require 'soft-morning-theme)

;;; Cask libraries.

(require 'auto-complete)
(require 'ac-sly)
(require 'company)
(require 'dash-at-point)
(require 'drag-stuff)
(require 'elpy)
(require 'expand-region)
(require 'fill-column-indicator)
(require 'flx-isearch)
(require 'haskell-mode)
(require 'magit)
(require 'markdown-mode)
(require 'mmm-mode)
(require 'page-break-lines)
(require 'paredit)
(require 'paren-face)
(require 'powerline)
(require 'projectile)
(require 'rainbow-mode)
(require 'sly)
(require 'vlf)
(require 'writeroom-mode)
(require 'yasnippet)

;;; Third-party libraries.
(require 'ac-company)
(when (display-graphic-p)
  (require 'clean-mode-line))

;;; Personal libraries.

(load-library "auto-scroll-to-end")
(load-library "osx-opening")
(load-library "sexp-manipulation")
(load-library "timestamps")
(load-library "window-manipulation")
(load-library "writing")

(load-library "keybinds")
(load-library "modes")
 
(load-library "customize") ; I keep customizes in a separate file.

;;; Powerline stuff
(when-graphical (require 'powerline))
(when-graphical (load-library "custom-powerline"))
(when-graphical (powerline-william-theme))

(when (fboundp 'toggle-frame-fullscreen)
  (toggle-frame-fullscreen))
