;;; org.el --- Org mode -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(global-set-key (kbd "C-c l") #'org-store-link)

(use-package org
  :custom (org-hide-leading-stars t))

;;; org.el ends here
