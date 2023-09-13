;; perl mode settings
(add-hook 'perl-mode-hook
          #'(lambda ()
              (require 'compile)
              (set (make-local-variable 'compile-command)
                   (concat "perl -c "
                           (file-name-nondirectory buffer-file-name)))))
