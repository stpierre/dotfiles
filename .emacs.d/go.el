(defun csp/golang-customizations ()
  (subword-mode 1)

  (setq display-fill-column-indicator-character ?\u2502)
  (display-fill-column-indicator-mode +1)

  (when (executable-find "gofumpt") (setq gofmt-command "gofumpt"))

  (gofmt-before-save))

(use-package go-mode
  :ensure t
  :config (setq tab-width 2
                standard-indent 2)
  :hook (go-mode . csp/golang-customizations))

(use-package gotest :ensure t)
