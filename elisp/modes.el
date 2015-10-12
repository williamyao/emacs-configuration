;;;; Configuration for various modes.

;;; Helm
(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

(defun helm-hide-modeline (source &optional force)
  (face-remap-add-relative 'mode-line 'mode-line-inactive)
  (setq mode-line-format "")
  (force-mode-line-update))

(advice-add 'helm-display-mode-line :override 'helm-hide-modeline)

(helm-mode 1)

(define-key helm-map (kbd "M-n") 'helm-next-source)
(define-key helm-map (kbd "M-p") 'helm-previous-source)

;;; Projectile
(projectile-global-mode 1)
(setq projectile-completion-system 'helm)

(define-key projectile-mode-map (kbd "M-p") 'helm-projectile)
    
;;; ERC
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(add-hook 'erc-mode-hook
	  (lambda ()
	    (erc-scrolltobottom-mode 1)
	    (setq erc-input-line-position -2)))

;;; FCI
(setq-default fci-rule-column 80)

;;; HS
(eval-after-load 'hideshow
  '(define-key hs-minor-mode-map (kbd "<C-tab>") 'hs-toggle-hiding))

;;; Text
(add-hook 'text-mode-hook
          (lambda ()
            (auto-fill-mode)
            (set-fill-column 72)))

(defun text-mode-wrap-beginning-of-defun (original &rest args)
  "Make `beginning-of-defun' behave like `backward-paragraph'
in `text-mode'."
  (if (memq major-mode '(text-mode writing-mode))
      (apply #'backward-paragraph args)
    (apply original args)))

(defun text-mode-wrap-end-of-defun (original &rest args)
  "Make `end-of-defun' behave like `forward-paragraph'
in `text-mode'."
  (if (memq major-mode '(text-mode writing-mode))
      (apply #'forward-paragraph args)
    (apply original args)))

(advice-add 'beginning-of-defun
            :around #'text-mode-wrap-beginning-of-defun)
(advice-add 'end-of-defun
            :around #'text-mode-wrap-end-of-defun)

(define-key text-mode-map (kbd "C-M-t") 'transpose-sentences)

;;; Paredit
(eval-after-load 'paredit
  '(progn
     ;; prevent clashes with DRAG-STUFF
     (define-key paredit-mode-map (kbd "<M-up>") nil)
     (define-key paredit-mode-map (kbd "<M-down>") nil)
     
     (define-key paredit-mode-map (kbd "C-w") 'paredit-backward-kill-word)

     ;; I literally never use this functionality of Paredit, and
     ;; it just gets in the way.
     (defun paredit-space-for-delimiter-p (endp delimiter)
       nil)))

;;; Lisp
(setq inferior-lisp-program "/usr/local/bin/ccl")

(defun lisp-mode-customization ()
  (paredit-mode 1)
  (abbrev-mode 1)
  (paren-face-mode 1)
  (hs-minor-mode 1)
  (fci-mode 1)
  (auto-complete-mode 1)

  (setup-tab-auto-complete)

  (hs-hide-all))

(ac-company-define-source ac-source-company-elisp company-elisp)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (lisp-mode-customization)
            (add-to-list 'ac-sources 'ac-source-company-elisp)))

;;; SLY
(add-hook 'sly-mode-hook
          (lambda ()
            (set-up-sly-ac t)))

(add-hook 'lisp-mode-hook 
	  (lambda ()
	    (sly-mode 1)
	    (lisp-mode-customization)))

(with-eval-after-load 'sly-mrepl
  (add-hook 'sly-mrepl-hook
            (lambda ()
              (paredit-mode 1)
              (abbrev-mode 1)
              (paren-face-mode 1)
              (auto-complete-mode 1)

              (setup-tab-auto-complete)))

 (define-key sly-mrepl-mode-map (kbd "C-i") 'indent-for-tab-command))

;;; Show Paren
(show-paren-mode 1)

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

(define-key c-mode-base-map (kbd "C-j") 'newline-and-indent)

(defun algol-like-customization ()
  (fci-mode 1)
  (hs-minor-mode 1)
  (page-break-lines-mode 1)
  (abbrev-mode 1)
  (auto-complete-mode 1)

  (setup-tab-auto-complete)

  (hs-hide-all))

(add-hook 'c-mode-hook 'algol-like-customization)
(add-hook 'c-mode-hook (lambda () (flymake-mode 1)))

(defvar make-clean-modes '(c-mode c++-mode)
  "List of modes in which to run 'make-clean' in before
`magit-status'.")

(defun maybe-make-clean (&rest args)
  (when (or (apply 'derived-mode-p make-clean-modes)
            (some (lambda (symbol)
                    (and (boundp symbol)
                         (symbol-value symbol)))
                  make-clean-modes))
    (shell-command "make clean")))

(advice-add 'magit-status :before 'maybe-make-clean)

;;; Dired
(setq-default dired-recursive-copies 'always)
(setq-default dired-recursive-deletes 'always)
(setq-default delete-by-moving-to-trash t)
;;; TODO 2015-09-12 williamyaoh@gmail.com
;;;  - Figure out what this would be on linux
(setq-default trash-directory (cl-case system-type
                                (darwin "~/.Trash/")
                                ((linux gnu/linux) "~/.local/share/Trash/")))

(setq-default dired-omit-files "^\\.")

(defun dired-customization ()
  (dired-hide-details-mode 1)
  (dired-omit-mode 1)
  
  (when (eql system-type 'darwin)
    (dired-osx-mode 1)))

(defun maybe-dired-find-file-other-window (original &rest args)
  "Wrapper for whatever the hell Dired is using at this particular
point in time for opening files."
  (if (file-directory-p (dired-get-file-for-visit))
      (apply original args)
    (dired-find-file-other-window)))

(defun eshell-send-text (text)
  "Send a command to the running eshell instance, or create
another one if eshell is not running."
  (require 'eshell)
  
  (unless (get-buffer eshell-buffer-name)
    (eshell))
  
  (let ((buffer (current-buffer)))
    (set-buffer eshell-buffer-name)
    (end-of-buffer)
    (eshell-kill-input)
    (insert text)
    (eshell-send-input)
    (set-buffer buffer)))

(add-hook 'dired-after-readin-hook
          (lambda ()
            (eshell-send-text (format "cd \"%s\"" default-directory))))

(add-hook 'dired-mode-hook 'dired-customization)
(add-hook 'dired-mode-hook 'dired-omit-mode)

(define-key dired-mode-map (kbd "C-o") 'dired-omit-mode)
(define-key dired-mode-map (kbd "e") nil)
(define-key dired-mode-map (kbd "f") nil)

(require 'dired+)

(advice-add 'diredp-find-file-reuse-dir-buffer
            :around 'maybe-dired-find-file-other-window)

(diredp-toggle-find-file-reuse-dir 1)

;;; HL Line
(global-hl-line-mode 1)

;;; Auto Complete
(setq-default ac-auto-start nil)
(setq-default ac-dwim nil)

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(setq completion-cycle-threshold 5)

(defun tab/auto-complete () #'auto-complete)

(defun setup-tab-auto-complete ()
  (add-to-list 'completion-at-point-functions 'tab/auto-complete))

;;; Hi Lock
(global-hi-lock-mode 1)

;;; Subword
(global-subword-mode 1)

;;; Python
(add-hook 'python-mode-hook
          (lambda ()
            (elpy-mode 1)))

(setq elpy-rpc-python-command "python3")
