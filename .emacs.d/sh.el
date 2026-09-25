;;; sh.el --- Shell scripts -*- lexical-binding: t -*-

;;; Commentary:
;;; Bash and sh scripts use `bash-ts-mode' (see treesit.el); other
;;; shells, like zsh, automatically fall back to `sh-mode'.

;;; Code:

(defun sh-customizations ()
  "Set up shell script buffers."
  ;; bash-language-server only understands bash and sh
  (when (and buffer-file-name (memq sh-shell '(bash sh)))
    (eglot-ensure)))

(add-hook 'sh-base-mode-hook #'sh-customizations)

;;; sh.el ends here
