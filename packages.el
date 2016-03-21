1;;;; Dyntu setup for various installed packages.

(use-package ido
  :config
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-create-new-buffer 'always))

(use-package smex
  :config
  (smex-initialize)
  (setq smex-prompt-string "> ")
  :bind
  (:map dyntu-global-map
	("M-x" . smex)
	("M-X" . smex-major-mode-commands)))

(use-package undo-tree
  :bind
  (:map dyntu-global-map
	("C--" . undo-tree-undo)
	("C-M--" . undo-tree-redo)
	("C-x C-M--" . undo-tree-visualize)))

(use-package recentf
  :config
  (recentf-mode t)
  (setq recentf-max-saved-items 50)
  (defun ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
	(message "Opening file...")
      (message "Aborting")))
  :bind
  (:map dyntu-global-map
	("C-x C-r" . ido-recentf-open)))
