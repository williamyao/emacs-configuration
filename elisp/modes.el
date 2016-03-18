;;;; Configuration for various modes.

;;; General programming customization.
(defun programming-customization ()
  (yas-minor-mode-on)
  (paren-face-mode 1)
  (hs-minor-mode 1)
  (fci-mode 1)
  (auto-complete-mode 1)
  (highlight-indentation-mode 1))

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

(setq helm-ff-newfile-prompt-p nil)

(helm-mode 1)

(define-key helm-map (kbd "M-n") 'helm-next-source)

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
  (lispy-william-mode 1)
  (yas-minor-mode-on)
  (paren-face-mode 1)
  (hs-minor-mode 1)
  (fci-mode 1)
  (auto-complete-mode 1)
  (highlight-indentation-mode 1)
  (highlight-indentation-set-offset 2)

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

(add-hook 'clojure-mode-hook 'lisp-mode-customization)

(with-eval-after-load 'sly-mrepl
  (add-hook 'sly-mrepl-hook
            (lambda ()
              (lispy-william-mode 1)
              (yas-minor-mode-on)
              (paren-face-mode 1)
              (auto-complete-mode 1)

              (setup-tab-auto-complete)))

  (define-key sly-mrepl-mode-map (kbd "C-i") 'indent-for-tab-command))

(defvar lispy-william-map (make-sparse-keymap))

;;; Global bindings
(define-key lispy-william-map (kbd "(") 'lispy-parens)
(define-key lispy-william-map (kbd ")") 'lispy-forward)
(define-key lispy-william-map (kbd "{") 'lispy-braces)
(define-key lispy-william-map (kbd "}") 'lispy-forward)
(define-key lispy-william-map (kbd "[") 'lispy-brackets)
(define-key lispy-william-map (kbd "]") 'lispy-forward)
(define-key lispy-william-map (kbd "\"") 'lispy-doublequote)
(define-key lispy-william-map (kbd "DEL") 'lispy-delete-backward)
(define-key lispy-william-map (kbd "M-DEL") 'lispy-backward-kill-word)
(define-key lispy-william-map (kbd "C-e") 'lispy-move-end-of-line)
(define-key lispy-william-map (kbd "C-d") 'lispy-delete)
(define-key lispy-william-map (kbd "C-k") 'lispy-kill)
(define-key lispy-william-map (kbd "M-k") 'lispy-kill-sentence)
(define-key lispy-william-map (kbd "M-m") 'lispy-mark-symbol)
(define-key lispy-william-map (kbd "C-,") 'lispy-kill-at-point)
(define-key lispy-william-map (kbd "C-1") 'lispy-describe-inline)
(define-key lispy-william-map (kbd "C-2") 'lispy-arglist-inline)
(define-key lispy-william-map (kbd "C-3") 'lispy-right)
(define-key lispy-william-map (kbd "RET") 'lispy-newline-and-indent-plain)

(define-key lispy-william-map (kbd "C-7") 'lispy-cursor-down)

(define-key lispy-william-map (kbd "M-n") 'lispy-ace-paren)

;;; Local bindings
(lispy-define-key lispy-william-map (kbd "a") 'lispy-left)
(lispy-define-key lispy-william-map (kbd "o") 'lispy-up)
(lispy-define-key lispy-william-map (kbd "e") 'lispy-down)
(lispy-define-key lispy-william-map (kbd "u") 'lispy-right)
(lispy-define-key lispy-william-map (kbd "d") 'lispy-different)
(lispy-define-key lispy-william-map (kbd "j") 'lispy-ace-char)
(lispy-define-key lispy-william-map (kbd "f") 'lispy-flow)

(defun lispy-william-space ()
  "Insert a space. Move back if at left paren."
  (interactive)
  (insert " ")
  (when (lispy-left-p)
    (left-char 1)))

(lispy-define-key lispy-william-map "SPC" 'lispy-william-space)

(lispy-define-key lispy-william-map (kbd "i") 'lispy-tab)
(lispy-define-key lispy-william-map (kbd "r") 'lispy-eval)

(lispy-define-key lispy-william-map (kbd "x") 'lispy-x)

(lispy-define-key lispy-william-map (kbd "q") 'lispy-ace-paren)
(lispy-define-key lispy-william-map (kbd "s") 'lispy-ace-symbol)

(lispy-define-key lispy-william-map (kbd "n") 'lispy-undo)
(lispy-define-key lispy-william-map (kbd "c") 'lispy-clone)
(lispy-define-key lispy-william-map (kbd "m") 'lispy-mark-list)
(lispy-define-key lispy-william-map (kbd "y") 'lispy-new-copy)
(lispy-define-key lispy-william-map (kbd "h") 'lispy-raise)
(lispy-define-key lispy-william-map (kbd "p") 'lispy-paste)

(defun lispy-william-movel (arg)
  (interactive "p")
  (cond ((lispy-right-p) (lispy-barf arg))
        ((lispy-left-p) (lispy-slurp arg))
        (t (self-insert-command 1))))

(defun lispy-william-mover (arg)
  (interactive "p")
  (cond ((lispy-right-p) (lispy-slurp arg))
        ((lispy-left-p) (lispy-barf arg))
        (t (self-insert-command 1))))

(define-key lispy-william-map (kbd "<") 'lispy-william-movel)
(define-key lispy-william-map (kbd ">") 'lispy-william-mover)
(lispy-define-key lispy-william-map (kbd ",") 'lispy-up-slurp)
(lispy-define-key lispy-william-map (kbd ".") 'lispy-down-slurp)

(lispy-define-key lispy-william-map (kbd "l") 'lispy-view)

(lispy-define-key lispy-william-map (kbd "C") 'lispy-convolute)
(lispy-define-key lispy-william-map (kbd "M") 'lispy-alt-multiline)
(lispy-define-key lispy-william-map (kbd "N") 'lispy-narrow)
(lispy-define-key lispy-william-map (kbd "W") 'lispy-widen)
(lispy-define-key lispy-william-map (kbd "S") 'lispy-splice)
(lispy-define-key lispy-william-map (kbd "R") 'lispy-eval-and-replace)

(lispy-define-key lispy-william-map (kbd "A") 'lispy-move-left)
(lispy-define-key lispy-william-map (kbd "O") 'lispy-move-up)
(lispy-define-key lispy-william-map (kbd "E") 'lispy-move-down)
(lispy-define-key lispy-william-map (kbd "U") 'lispy-move-right)

(lispy-define-key lispy-william-map (kbd "1") 'digit-argument)
(lispy-define-key lispy-william-map (kbd "2") 'digit-argument)
(lispy-define-key lispy-william-map (kbd "3") 'digit-argument)
(lispy-define-key lispy-william-map (kbd "4") 'digit-argument)
(lispy-define-key lispy-william-map (kbd "5") 'digit-argument)
(lispy-define-key lispy-william-map (kbd "6") 'digit-argument)
(lispy-define-key lispy-william-map (kbd "7") 'digit-argument)
(lispy-define-key lispy-william-map (kbd "8") 'digit-argument)
(lispy-define-key lispy-william-map (kbd "9") 'digit-argument)
(lispy-define-key lispy-william-map (kbd "0") 'digit-argument)

(define-minor-mode lispy-william-mode
  "Wrapper minor mode to have sane keymapping."
  :keymap lispy-william-map
  :group 'lispy
  :lighter " (L)")

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
  (yas-minor-mode-on)
  (auto-complete-mode 1)

  (setup-tab-auto-complete)

  (hs-hide-all))

(add-hook 'c-mode-hook 'algol-like-customization)

(add-hook 'c++-mode-hook 'algol-like-customization)

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

;;; TeX
(add-hook 'tex-mode-hook
          (lambda ()
            (yas-minor-mode-on)))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (yas-minor-mode-on)))

;;; TRAMP
(setq tramp-default-method "ssh")

;;; YASnippet
(yas-reload-all)

;;; MMM
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 0)
