;;; xml.el --- XML -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun nxml-customizations ()
  "Set up `nxml-mode' buffers."
  (setq-local tab-width 2)
  (when buffer-file-name
    (setq-local compile-command
                (concat "xmllint --noout "
                        (shell-quote-argument
                         (file-name-nondirectory buffer-file-name))))))

(use-package nxml-mode
  :custom
  (nxml-slash-auto-complete-flag t)
  (nxml-auto-insert-xml-declaration-flag t)
  :hook (nxml-mode . nxml-customizations))

;;; xml.el ends here
