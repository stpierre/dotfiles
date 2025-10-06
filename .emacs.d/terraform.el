(use-package
 terraform-mode
 :ensure t
 :hook (terraform-mode . terraform-format-on-save-mode))
