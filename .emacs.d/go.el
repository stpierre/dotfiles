;;; go.el --- Go -*- lexical-binding: t -*-

;;; Commentary:
;;; Go uses the built-in `go-ts-mode' and gopls.  gopls formats with
;;; gofumpt (see eglot-workspace-configuration in prog.el).  Files keep
;;; real tabs, as gofmt requires; they're just displayed 2 columns wide.

;;; Code:

(defun golang-customizations ()
  "Set up `go-ts-mode' buffers."
  (setq-local tab-width 2)
  (subword-mode 1)
  (when buffer-file-name
    (eglot-ensure)
    (add-hook 'before-save-hook #'format-and-organize-imports nil t)))

(use-package go-ts-mode
  ;; must match tab-width, or each indent level gets two tabs
  :custom (go-ts-mode-indent-offset 2)
  :hook (go-ts-mode . golang-customizations))

;;; go.el ends here
