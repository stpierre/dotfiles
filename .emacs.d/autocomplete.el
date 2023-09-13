(use-package auto-complete-config
  :ensure auto-complete
  :diminish auto-complete-mode
  :config (progn
            (add-to-list 'ac-dictionary-directories
                         (concat user-emacs-directory "/ac-dict"))
            (ac-config-default)
            (define-key ac-completing-map [return] nil)
            (define-key ac-completing-map "\r" nil)))
(use-package auto-complete-nxml :ensure t)
(use-package auto-complete-rst :ensure t)
