;;; miscellaneous documentation format settings

;; automatically include licenses
(use-package legalese :ensure t)

(use-package markdown-mode
  :ensure t
  :hook (rst-mode . (lambda () (flyspell-mode 1))))

(add-hook 'rst-mode-hook
          #'(lambda () (flyspell-mode 1)))

;; graphviz mode settings
(use-package graphviz-dot-mode
  :ensure t
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\'" . graphviz-dot-mode))
  :hook (graphviz-dot-mode . (lambda ()
                               (setq tab-width 4)
                               (setq graphviz-dot-indent-width 4)
                               (setq graphviz-dot-auto-indent-on-newline nil)
                               (setq graphviz-dot-auto-indent-on-braces nil)
                               (setq graphviz-dot-auto-indent-on-semi nil))))
