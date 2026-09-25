;;; sql.el --- SQL -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun sql-customizations ()
  "Set up `sql-mode' buffers."
  (setq-local tab-width 2)
  (sqlformat-on-save-mode 1))

(use-package sqlformat
  :ensure t
  :hook (sql-mode . sql-customizations))

;;; sql.el ends here
