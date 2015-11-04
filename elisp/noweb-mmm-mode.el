;;;; Minor mode for editing Noweb files using MMM.

(require 'mmm-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nw\\'" . noweb-mmm-mode))

;;;###autoload
(define-minor-mode noweb-mmm-mode
  "Minor mode for editing Noweb files. Provides shortcuts for editing
files, as well as automatic detection of appropriate modes."
  (if noweb-mmm-mode
      (noweb-mmm-mode-on)
    (noweb-mmm-mode-off)))

(defun remove-noweb-extension (filename)
  (if (string-match "\\.nw\\'\\|\\.noweb\\'" filename)
      (replace-match "" nil nil filename)
    filename))

(defun noweb-mmm-mode-get-buffer-submode ()
  (when (buffer-file-name)
    (let ((filename (remove-noweb-extension (buffer-file-name))))
      (assoc-default filename
                     auto-mode-alist
                     'string-match))))

(defun noweb-mmm-mode-on ()
  (interactive)
  (latex-mode)
  (mmm-mode-on)
  (mmm-ify-by-regexp (noweb-mmm-mode-get-buffer-submode)
                     "^<<.+?>>="
                     'end-of-line
                     "^@ \\|^@\n\\|\\'"
                     'end-of-line
                     nil))

(defun noweb-mmm-mode-insert-region (name)
  (interactive `(,(read-string "Code region name (default *): " nil nil "*")))
  (insert "<<" name ">>=\n")
  (mmm-ify-region (noweb-mmm-mode-get-buffer-submode)
                  (- (point) 1)
                  (+ (point) 1)))

(defun noweb-mmm-mode-off ()
  (mmm-mode-off))

(define-key mmm-mode-map (kbd "C-%") 'noweb-mmm-mode-insert-region)
