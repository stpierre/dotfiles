;;; prog.el --- Settings shared by all programming modes -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun prog-mode-customizations ()
  "Set up `prog-mode' buffers."
  (setq-local show-trailing-whitespace t)

  ;; wrap comments, but not code
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1)

  (display-fill-column-indicator-mode 1))

(add-hook 'prog-mode-hook #'prog-mode-customizations)

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-workspace-configuration '(:gopls (:gofumpt t))))

(defun format-and-organize-imports ()
  "Format the buffer and organize imports with eglot.

Meant for `before-save-hook'.  Errors are reported but never block
the save, and servers that can't do one or the other are skipped."
  (when (eglot-managed-p)
    (condition-case err
        (eglot-code-action-organize-imports (point-min) (point-max))
      (error
       ;; gopls offers no action at all when imports are already tidy
       (unless (string-match-p "No \"source.organizeImports\" code actions"
                               (error-message-string err))
         (message "Organize imports failed: %s"
                  (error-message-string err)))))
    (condition-case err
        (eglot-format-buffer)
      (error (message "Formatting failed: %s"
                      (error-message-string err))))))

(use-package xref
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

;; fallback for when there's no language server
(use-package dumb-jump
  :ensure t
  :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;; prog.el ends here
