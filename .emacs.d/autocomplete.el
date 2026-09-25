;;; autocomplete.el --- In-buffer completion -*- lexical-binding: t -*-

;;; Commentary:
;;; Corfu popup everywhere, with extra completion sources from cape.

;;; Code:

(use-package corfu
  :ensure t
  :custom (corfu-auto t)
  :hook (after-init . global-corfu-mode))

;; corfu needs child frames, which TTYs only get in Emacs 31
(use-package corfu-terminal
  :ensure t
  :if (< emacs-major-version 31)
  :hook (after-init . corfu-terminal-mode))

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

;;; autocomplete.el ends here
