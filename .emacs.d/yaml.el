;;; yaml.el --- YAML -*- lexical-binding: t -*-

;;; Commentary:
;;; YAML uses the built-in `yaml-ts-mode' (see treesit.el).  It derives
;;; from text-mode, not prog-mode; text.el turns off full-buffer spell
;;; checking for it.

;;; Code:

(defun yaml-customizations ()
  "Set up `yaml-ts-mode' buffers."
  (when buffer-file-name
    (eglot-ensure))
  (subword-mode 1)
  (display-fill-column-indicator-mode 1))

(add-hook 'yaml-ts-mode-hook #'yaml-customizations)

;;; yaml.el ends here
