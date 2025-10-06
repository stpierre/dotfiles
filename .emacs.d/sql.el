(defun sql-customizations ()
  (setq sqlformat-command 'sqlformat)
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  (sqlformat-on-save-mode))

(use-package sqlformat :ensure t)

(add-hook 'sql-mode-hook 'sql-customizations)

