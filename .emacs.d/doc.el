;;; doc.el --- Documentation formats -*- lexical-binding: t -*-

;;; Commentary:
;;; Miscellaneous documentation format settings.

;;; Code:

;; automatically include licenses
(use-package legalese :ensure t)

(use-package markdown-mode :ensure t)

(defun graphviz-customizations ()
  "Set up `graphviz-dot-mode' buffers."
  (setq-local tab-width 4))

(use-package graphviz-dot-mode
  :ensure t
  :custom
  (graphviz-dot-indent-width 4)
  (graphviz-dot-auto-indent-on-newline nil)
  (graphviz-dot-auto-indent-on-braces nil)
  (graphviz-dot-auto-indent-on-semi nil)
  :hook (graphviz-dot-mode . graphviz-customizations))

;;; doc.el ends here
