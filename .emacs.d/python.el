(defun python-customizations ()
  (eglot-ensure)

  (add-hook 'before-save-hook #'python-format-on-save nil t)

  (setq python-fill-docstring-style 'pep-257-nn)
  (setq tab-width 4)
  (setq python-indent 4))

(use-package python
  :ensure python-mode
  :mode ("\\.wsgi" . python-mode)
  :config
  (defun python-disable-qa-on-line ()
    "Disable python QA on the current line."
    (interactive)
    (end-of-line)
    (insert "  # noqa"))

  (defun python-disable-cover-on-line ()
    "Disable checking for unit test coverage on the current line."
    (interactive)
    (end-of-line)
    (insert "  # pragma: nocover"))

  (defun python-disable-typing-on-line ()
    "Disable type checking on the current line."
    (interactive)
    (end-of-line)
    (insert "  # type: ignore"))

  (defun python-disable-errors-on-line ()
    (interactive)
    (end-of-line)
    (let ((error-codes (flymake-get-error-codes-on-line)))
      (if (> (length error-codes) 0)
          (insert
           (concat "  # noqa: " (string-join error-codes ","))))))

  (defun python-format-on-save ()
    (call-interactively 'eglot-code-action-organize-imports)
    (eglot-format-buffer))

  (define-key python-mode-map (kbd "C-c e c") #'disable-cover-on-line)
  (define-key python-mode-map (kbd "C-c e d") #'disable-errors-on-line)
  (define-key python-mode-map (kbd "C-c e q") #'disable-qa-on-line)
  (define-key python-mode-map (kbd "C-c e t") #'disable-typing-on-line)

  :hook (python-mode . python-customizations))

;; create a python-scratch buffer that's just like *scratch*, but with
;; the python major mode
(with-current-buffer (generate-new-buffer "*python-scratch*")
  (python-mode))
