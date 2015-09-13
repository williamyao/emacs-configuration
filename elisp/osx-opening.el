;;;; Using dired to open things using OSX applications.

(when (eql system-type 'darwin)
  (defun dired-osx-open (&optional arg file-list)
    (interactive
     (list current-prefix-arg (dired-get-marked-files t current-prefix-arg)))
    (dired-do-shell-command "open" arg file-list))

  (defun dired-osx-open-directory-in-finder ()
    (interactive)
    (dired-do-shell-command "open" nil (list ".")))

  (defvar dired-osx-mode-map (make-sparse-keymap))
  (defvar dired-osx-lighter " OSX")

  (define-key dired-osx-mode-map (kbd "<C-return>") 'dired-osx-open)
  (define-key dired-osx-mode-map (kbd "<C-M-return>") 'dired-osx-open-directory-in-finder)

  (define-minor-mode dired-osx-mode
    "Minor mode for integrating Dired with OSX Finder and
opening files using other OSX applications."
    nil
    dired-osx-lighter
    dired-osx-mode-map))
