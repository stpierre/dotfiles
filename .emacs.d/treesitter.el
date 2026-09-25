;;; treesitter.el --- Tree-sitter major modes -*- lexical-binding: t -*-

;;; Commentary:
;;; Use the built-in *-ts-mode for these languages, and offer to install
;;; a grammar the first time one is needed.  To go back to the old mode
;;; for one language, drop it from `treesit-auto-langs' and reinstall
;;; the old package.

;;; Code:

(use-package treesit-auto
  :ensure t
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs '(bash dockerfile go gomod json python toml yaml))
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

;;; treesitter.el ends here
