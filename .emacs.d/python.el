(defun python-customizations ()
  (eglot-ensure))

(use-package
 python
 :ensure python-mode
 :mode ("\\.wsgi" . python-mode)
 :config
 (setq
  python-fill-docstring-style 'pep-257-nn
  tab-width 4
  python-indent 4)

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

 (define-key python-mode-map (kbd "C-e c") #'disable-cover-on-line)
 (define-key python-mode-map (kbd "C-e d") #'disable-errors-on-line)
 (define-key python-mode-map (kbd "C-e q") #'disable-qa-on-line)
 (define-key python-mode-map (kbd "C-e t") #'disable-typing-on-line)

 :hook
 (python-mode . python-customizations)
 (before-save . python-format-on-save))

;; create a python-scratch buffer that's just like *scratch*, but with
;; the python major mode
(with-current-buffer (generate-new-buffer "*python-scratch*")
  (python-mode))
