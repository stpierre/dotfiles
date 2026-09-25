;;; js.el --- JavaScript and CSS -*- lexical-binding: t -*-

;;; Commentary:
;;; `css-mode' also provides `scss-mode'.

;;; Code:

(use-package js
  :custom (js-indent-level 2))

(use-package css-mode
  :custom
  (css-indent-offset 2)
  ;; colorful-mode (see theme.el) already shows colors
  (css-fontify-colors nil))

;;; js.el ends here
