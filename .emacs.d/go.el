(defun golang-customizations ()
  (eglot-ensure)
  
  (add-hook 'before-save-hook #'gofmt nil t)
  
  (subword-mode 1)

  (setq tab-width 2)
  (setq standard-indent 2)
  
  (when (executable-find "gofumpt")
    (setq gofmt-command "gofumpt")))

(use-package go-mode
  :ensure t
  :hook (go-mode . golang-customizations))
