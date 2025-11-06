;; yaml isn't a prog-mode, so we have to repeat ourselves a bit here
(defun yaml-customizations ()
  (eglot-ensure)

  (subword-mode 1)
  
  (define-key yaml-mode-map (kbd "C-c c") 'comment-region)
  (define-key yaml-mode-map (kbd "C-c u") 'uncomment-region)

  (setq display-fill-column-indicator-character ?\u2502)
  (display-fill-column-indicator-mode +1))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'"
  :hook
  (yaml-mode . yaml-customizations))
