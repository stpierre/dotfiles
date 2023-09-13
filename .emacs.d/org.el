(add-hook 'org-mode-hook
          #'(lambda ()
              (local-set-key "\C-cl" 'org-store-link)))

(setq org-hide-leading-stars t)
