;;;; Dyntu setup for various installed packages.


;;; Programming languages.

(use-package emacs
  :config
  (add-hook 'emacs-lisp-mode-hook 'programming-customization)
  (add-hook 'emacs-lisp-mode-hook 'lisp-customization)
  :mode
  ("Cask\\'" . emacs-lisp-mode))
(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook 'programming-customization)
  (add-hook 'clojure-mode-hook 'lisp-customization)
  (add-hook 'clojure-mode-hook (lambda () (eldoc-mode 1))))
(use-package cider)
(use-package d-mode
  :config
  (add-hook 'd-mode-hook 'programming-customization))
(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'programming-customization))
(use-package lisp-mode
  :config
  (add-hook 'lisp-mode-hook 'programming-customization)
  (add-hook 'lisp-mode-hook 'lisp-customization))
(use-package cc-mode
  :config
  (setq-default c-basic-offset 4)
  (setq-default c-default-style "linux")
  (add-hook 'c-mode-hook 'programming-customization)
  (add-hook 'c++-mode-hook 'programming-customization))
(use-package sml-mode
  :config
  (add-hook 'sml-mode-hook 'programming-customization))
(use-package sly
  :config
  (setq-default inferior-lisp-program "sbcl"))
(use-package typescript-mode)

(use-package ido
  :config
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-create-new-buffer 'always))

(use-package smex
  :config
  (smex-initialize)
  (setq smex-prompt-string "> "))

(use-package undo-tree)

(use-package recentf
  :config
  (recentf-mode t)
  (setq recentf-max-saved-items 50)
  (defun ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
	(message "Opening file...")
    (message "Aborting"))))

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

(use-package magit)

(use-package discover)

(use-package vlf)

(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

(use-package fill-column-indicator
  :config
  (setq-default fci-rule-column 80)
  (advice-add 'programming-customization
              :after
              (lambda () (fci-mode 1))))

(use-package hideshow
  :config
  (advice-add 'programming-customization
              :after
              (lambda ()
                (hs-minor-mode 1)
                (hs-hide-all))))

(use-package lispy
  :config
  (advice-add 'lisp-customization
              :after
              (lambda ()
                (lispy-mode 1))))

(use-package paredit)

(use-package vlf)

(use-package doc-view
  :bind
  (:map doc-view-mode-map
        ("M--" . doc-view-shrink)
        ("M-=" . doc-view-enlarge)
        ("M-g M-g" . doc-view-goto-page)))

(use-package ledger-mode
  :mode ".*ledger.*\\.dat\\'")

(use-package erc
  :config
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (add-hook 'erc-mode-hook
            (lambda ()
              (erc-scrolltobottom-mode 1)
              (setq erc-input-line-position -2))))

(use-package multiple-cursors)
