(setq auto-mode-alist (append '(("\\.xsd\\'" . xml-mode))
                              auto-mode-alist))
(setq auto-mode-alist (append '(("\\.rng\\'" . xml-mode))
                              auto-mode-alist))
(add-hook 'nxml-mode-hook
          #'(lambda ()
              (setq tab-width 2)
              (setq nxml-child-indent tab-width)
              (setq nxml-slash-auto-complete-flag t)
              (setq nxml-auto-insert-xml-declaration-flag t)
              (require 'compile)
              (set (make-local-variable 'compile-command)
                   (concat "xmllint --noout "
                           (file-name-nondirectory buffer-file-name)))))

(add-hook 'sgml-mode-hook 'turn-on-auto-fill)
