(use-package terraform-mode
  :ensure t
  :hook (terraform-mode-hook . terraform-format-on-save-mode))
