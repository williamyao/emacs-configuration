;;;; Configuration for various modes.

;;;; Updated 2015-08-29

;;; IDO
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order
      '(".lisp" ".md" ".asd"))
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;; ERC
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(add-hook 'erc-mode-hook
	  (lambda ()
	    (erc-scrolltobottom-mode 1)
	    (setq erc-input-line-position -2)))

;;; Text
(add-hook 'text-mode-hook (lambda () (visual-line-mode 1)))

;;; Company
(eval-after-load 'company
  '(progn
     (add-hook 'company-completion-started-hook
               (lambda (a)
                 (fci-mode 0)
                 (page-break-lines-mode 0)))
     (add-hook 'company-completion-cancelled-hook
               (lambda (a)
                 (fci-mode 1)
                 (page-break-lines-mode 1)))
     (add-hook 'company-completion-finished-hook
               (lambda (a)
                 (fci-mode 1)
                 (page-break-lines-mode 1)))

     (define-key company-mode-map (kbd "C-M-i") 'company-complete)

     (global-company-mode 1)))

;;; FCI
(setq-default fci-rule-column 80)

;;; HS
(eval-after-load 'hideshow
  '(define-key hs-minor-mode-map (kbd "<C-tab>") 'hs-toggle-hiding))

;;; Paredit
(eval-after-load 'paredit
  '(progn
     ;; prevent clashes with DRAG-STUFF
     (define-key paredit-mode-map (kbd "<M-up>") nil)
     (define-key paredit-mode-map (kbd "<M-down>") nil)
     
     (define-key paredit-mode-map (kbd "C-w") 'paredit-backward-kill-word)))

;;; Lisp
(setq inferior-lisp-program "/usr/local/bin/ccl")

(defun lisp-mode-customization ()
  (paredit-mode 1)
  (abbrev-mode 1)
  (paren-face-mode 1)
  (hs-minor-mode 1)
  (projectile-mode 1)
  (fci-mode 1)

  (hs-hide-all))

(add-hook 'emacs-lisp-mode-hook 'lisp-mode-customization)

;;; SLY
(add-hook 'sly-mode-hook 'sly-company-mode)
(add-to-list 'company-backends 'sly-company)

(add-hook 'lisp-mode-hook 
	  (lambda ()
	    (sly-mode 1)
	    (lisp-mode-customization)))

;;; Show Paren
(show-paren-mode 1)

;;; Eshell
(add-hook 'eshell-mode-hook 
	  (lambda ()
	    (when (member system-type '(cygwin linux gnu/linux darwin))
	      (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))
	      (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH"))))))

;;; Rainbow
(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (rainbow-mode 1)))

(global-rainbow-mode 1)

;;; Pending Delete
(define-globalized-minor-mode global-delete-selection-mode delete-selection-mode
  (lambda ()
    (delete-selection-mode 1)))

(global-delete-selection-mode 1)

;;; ISearch 
(defadvice isearch-repeat (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)))

;;; Drag Stuff
(define-globalized-minor-mode global-drag-stuff-mode drag-stuff-mode
  (lambda ()
    (drag-stuff-mode 1)))

(global-drag-stuff-mode 1)

;;; Page Break Lines mode
(global-page-break-lines-mode)

;;; CC
(require 'cc-mode)
(setq-default c-basic-offset 4
              c-default-style "linux")

(modify-syntax-entry ?\< "(>" c-mode-syntax-table)
(modify-syntax-entry ?\> ")<" c-mode-syntax-table)

(define-key c-mode-base-map (kbd "C-j") 'newline-and-indent)

(defun algol-like-customization ()
  (fci-mode 1)
  (hs-minor-mode 1)
  (projectile-mode 1)
  (abbrev-mode 1)

  (hs-hide-all))

(add-hook 'c-mode-hook 'algol-like-customization)
(add-hook 'c-mode-hook (lambda () (flymake-mode 1)))
