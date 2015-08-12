;;;; Configuration for various modes.

;;;; Updated 2015-08-05

;;; IDO
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order
      '(".lisp" ".md" ".asd"))

;;; ERC
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(add-hook 'erc-mode-hook
	  (lambda ()
	    (erc-scrolltobottom-mode)
	    (setq erc-input-line-position -2)))

;;; Text
(add-hook 'text-mode-hook (lambda () (visual-line-mode)))

;;; Lisp

(setq inferior-lisp-program "/usr/local/bin/ccl")

(defun lisp-mode-customization ()
  (paredit-mode)
  (abbrev-mode)
  (paren-face-mode)
  (hs-minor-mode)
  (company-mode)
  
  (column-marker-1 75)

  (set-face-foreground 'column-marker-1 "red")
  (set-face-background 'column-marker-1 nil)

  (local-set-key (kbd "C-w") 'paredit-backward-kill-word)
  (local-set-key (kbd "C-M-i") 'company-complete)
  (local-set-key (kbd "<C-tab>") 'hs-toggle-hiding)

  (hs-hide-all))

(add-hook 'emacs-lisp-mode-hook 'lisp-mode-customization)

;;; SLY
(add-hook 'sly-mode-hook 'sly-company-mode)
(add-to-list 'company-backends 'sly-company)

(add-hook 'lisp-mode-hook 
	  (lambda ()
	    (sly-mode)
	    (lisp-mode-customization)))

;;; Show Paren
(show-paren-mode 1)

;;; Eshell
(add-hook 'eshell-mode-hook 
	  (lambda ()
	    (when (member system-type '(cygwin linux gnu/linux darwin))
	      (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))
	      (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH"))))))

;;; Rainbow mode
(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (rainbow-mode)))

(global-rainbow-mode)

;;; Pending Delete mode
(define-globalized-minor-mode global-delete-selection-mode delete-selection-mode
  (lambda ()
    (delete-selection-mode)))

(global-delete-selection-mode)
