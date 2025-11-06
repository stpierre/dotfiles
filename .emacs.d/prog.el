(defun prog-mode-customizations ()
  (define-key prog-mode-map (kbd "C-c c") 'comment-region)
  (define-key prog-mode-map (kbd "C-c u") 'uncomment-region)

  (auto-fill-mode nil)

  (setq display-fill-column-indicator-character ?\u2502)
  (display-fill-column-indicator-mode +1))

(add-hook 'prog-mode-hook #'prog-mode-customizations)

(eval-after-load 'flymake
  '(progn
     (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
     (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)))
