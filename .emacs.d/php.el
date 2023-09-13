(add-hook 'php-mode-hook
          #'(lambda ()
             (setq tab-width 4)
             (require 'compile)
             (set (make-local-variable 'compile-command)
                  (concat "php -l "
                          (file-name-nondirectory buffer-file-name)))))
