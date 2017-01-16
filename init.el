;;;; Dyntu setup.

(add-to-list 'load-path "~/.emacs.d/elisp")

(load-library "~/.emacs.d/setup-cask.el")

;;;; - Keybindings -------------------------------------------------------------
;;;                 Edit `keybindsings.el' to change global keybinds.

(load-library "~/.emacs.d/keybindings.el")
;;;; ---------------------------------------------------------------------------

;;;; - Programming customization -----------------------------------------------

(defun programming-customization ()
  "Called when entering a programming mode. General programming packages
   should add advice after this function. Programming modes should add this
   as a hook alongside whatever other hooks they need to run."
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))
(defun lisp-customization ()
  "Called when entering a lisp mode. Lisp packages should add advice
   after this function.")

;;;; ---------------------------------------------------------------------------

;;;; - Color theme -------------------------------------------------------------

(load-theme 'quasi-monochrome t)
;;;; ---------------------------------------------------------------------------

;;;; - Package installation ----------------------------------------------------
;;;                 Edit `packages.el' to change package customization.

(load-library "~/.emacs.d/packages.el")
;;;; ---------------------------------------------------------------------------

;;;; - Customizes --------------------------------------------------------------
;;;                 Edit `customize.el' to change customizes.

(setq custom-file "~/.emacs.d/customize.el")
(load custom-file)
;;;; ---------------------------------------------------------------------------

;;;; - General stuff -----------------------------------------------------------

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq default-left-margin-width nil)
(setq default-right-margin-width nil)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-message t)

(global-subword-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq confirm-nonexistant-file-or-buffer nil)

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
	kill-buffer-query-functions))

(tooltip-mode -1)
(setq tooltip-use-echo-area t)

(setq ring-bell-function (lambda ()))
(setq show-help-function nil)

(setq-default sentence-end "[\\.;:!?] ")
(setq-default make-backup-files nil)

(setq-default truncate-lines t)

(define-globalized-minor-mode global-delete-selection-mode
  delete-selection-mode
  (lambda ()
    (delete-selection-mode 1)))

(global-delete-selection-mode 1)

(show-paren-mode 1)

(setq enable-recursive-minibuffers t)

(require 'clean-mode-line)

;;; Tabulation. Important enough for a tag.

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;;;; ---------------------------------------------------------------------------
