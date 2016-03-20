;;;; Extremely custom keybindings that
;;;;   a) don't depend on any packages being installed;
;;;;      any keybindings that do so should be factored out into
;;;;      the specific customization for that mode
;;;;   b) are easy to use on a *Dvorak* keyboard

(defvar dyntu-global-map (make-keymap))

(use-global-map dyntu-global-map)
