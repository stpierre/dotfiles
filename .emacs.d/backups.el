(setq
 delete-old-versions t
 version-control t
 backup-by-copying t)

;; make numbered backups in ~/.emacs.d/backups/
(use-package backup-each-save :ensure t)
(let ((backups (concat user-emacs-directory
                       (convert-standard-filename "backups/"))))
  (setq backup-directory-alist (cons (cons "." backups) nil))

  ;; ensure that backup directory exists
  (if (not (file-exists-p backups))
      (if (file-regular-p backups)
          (message (concat backups " exists, but is not a directory"))
        (make-directory backups))))
