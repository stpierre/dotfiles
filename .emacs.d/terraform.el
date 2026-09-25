;;; terraform.el --- Terraform -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package terraform-mode
  :ensure t
  :hook (terraform-mode . terraform-format-on-save-mode))

;;; terraform.el ends here
