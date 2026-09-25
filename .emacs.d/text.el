;;; text.el --- Settings shared by all text modes -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun text-mode-customizations ()
  "Set up `text-mode' buffers."
  (setq-local show-trailing-whitespace t)
  (auto-fill-mode 1)
  (if (derived-mode-p '(yaml-ts-mode nxml-mode))
      ;; these are really code, even though they derive from text-mode
      (progn
        (setq-local comment-auto-fill-only-comments t)
        (flyspell-prog-mode))
    (flyspell-mode 1)))

(add-hook 'text-mode-hook #'text-mode-customizations)

;;; text.el ends here
