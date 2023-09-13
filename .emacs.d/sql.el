(use-package sqlformat :ensure t)

(add-hook 'sql-mode-hook
          #'(lambda ()
              (setq sqlformat-command 'sqlformat)
              (setq tab-width 2)
              (setq indent-tabs-mode nil)
              (sqlformat-on-save-mode)))
