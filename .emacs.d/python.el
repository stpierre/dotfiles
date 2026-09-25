;;; python.el --- Python -*- lexical-binding: t -*-

;;; Commentary:
;;; Built-in python.el (`python-ts-mode' via treesit.el), with eglot.

;;; Code:

(defun python-customizations ()
  "Set up Python buffers."
  (setq-local tab-width 4)
  (when buffer-file-name
    (eglot-ensure)
    (add-hook 'before-save-hook #'format-and-organize-imports nil t)))

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
  "Add a noqa comment for the error codes flymake reports on this line."
  (interactive)
  (let ((codes
         (delete-dups
          (delq nil
                (mapcar
                 (lambda (diag)
                   (let ((code (plist-get
                                (alist-get 'eglot-lsp-diag
                                           (flymake-diagnostic-data diag))
                                :code)))
                     (and code (format "%s" code))))
                 (flymake-diagnostics (line-beginning-position)
                                      (line-end-position)))))))
    (if (not codes)
        (message "No error codes on this line")
      (end-of-line)
      (insert "  # noqa: " (string-join codes ",")))))

(use-package python
  :mode ("\\.wsgi\\'" . python-mode)
  :custom
  (python-fill-docstring-style 'pep-257-nn)
  (python-indent-guess-indent-offset-verbose nil)
  :hook (python-base-mode . python-customizations)
  :config
  ;; python-ts-mode-map is a copy of python-mode-map, so bind in both
  (dolist (map (list python-mode-map python-ts-mode-map))
    (define-key map (kbd "C-c e c") #'python-disable-cover-on-line)
    (define-key map (kbd "C-c e d") #'python-disable-errors-on-line)
    (define-key map (kbd "C-c e q") #'python-disable-qa-on-line)
    (define-key map (kbd "C-c e t") #'python-disable-typing-on-line)))

;;; python.el ends here
