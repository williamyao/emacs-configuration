;;;; Dyntu setup for various installed packages.

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

(use-package powerline
  :config
  (load-library "custom-powerline")
  (powerline-william-theme))

(use-package paren-face
  :config
  (global-paren-face-mode 1))

(use-package rainbow-mode
  :config
  (define-globalized-minor-mode global-rainbow-mode rainbow-mode
    (lambda ()
      (rainbow-mode 1)))

  (global-rainbow-mode 1))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

(use-package discover)

(use-package vlf)

(use-package clojure-mode)

(use-package cider)

(use-package d-mode)

(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

(use-package fill-column-indicator
  :config
  (setq-default fci-rule-column 80))

(use-package hideshow
  :bind
  (:map dyntu-global-map
        ("M-h" . hs-toggle-hiding)))

(use-package paredit)

(use-package sml-mode)

(use-package vlf)

(use-package sly)

(use-package cc-mode
  :config
  (setq-default c-basic-offset 4)
  (setq-default c-default-style "linux"))

(use-package doc-view
  :bind
  (:map doc-view-mode-map
        ("M--" . doc-view-shrink)
        ("M-=" . doc-view-enlarge)
        ("M-g M-g" . doc-view-goto-page)))
