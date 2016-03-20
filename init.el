;;;; Dyntu setup.

(load-library "~/.emacs.d/setup-cask.el")

;;;; - Keybindings -------------------------------------------------------------
;;;                 Edit `keybindsings.el' to change global keybinds.

(load-library "~/.emacs.d/keybindings.el")
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
;;;; ---------------------------------------------------------------------------
