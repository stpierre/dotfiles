;;; init.el --- .emacs customizations -*- lexical-binding: t -*-

;;; Commentary:
;;; Initial emacs startup bits. Most stuff should be organized into
;;; other files.

;;; Code:

(require 'package)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)

;; packages' autoloads cover almost everything, so only load a package
;; when it's first used
(setq use-package-always-defer t)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t t)

(use-package emacs
  :custom
  (auto-save-default nil)
  (auto-save-list-file-prefix nil)
  (make-backup-files nil)
  (echo-keystrokes 0.1)
  (imenu-auto-rescan t)
  (imenu-flatten 'annotation)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message "")
  (require-final-newline t)
  (use-short-answers t)
  (indent-tabs-mode nil)
  (fill-column 79)
  (tab-always-indent 'complete)
  (read-extended-command-predicate
   #'command-completion-default-include-p)
  (user-mail-address "chris.a.st.pierre@gmail.com")
  (user-full-name "Chris St. Pierre")
  (recentf-max-saved-items 200)
  (global-auto-revert-non-file-buffers t)
  :config
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)

  (save-place-mode 1)
  (savehist-mode 1)
  (recentf-mode 1)
  (global-auto-revert-mode 1)
  (which-key-mode 1)
  (editorconfig-mode 1)

  ;; chmod +x files that start with #!
  (add-hook 'after-save-hook
            #'executable-make-buffer-file-executable-if-script-p))

;; declare it special, so the let below binds it dynamically before
;; sort.el is loaded
(defvar sort-fold-case)

(defun sort-lines-nocase ()
  "Sort lines case-insensitively."
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

;; trim trailing whitespace, but only on lines that have been edited
(use-package ws-butler
  :ensure t
  :hook (after-init . ws-butler-global-mode))

;; M-x scratch opens a scratch buffer in the current major mode; C-u
;; M-x scratch prompts for the mode
(use-package scratch :ensure t)

;; load every other *.el file in this directory
(dolist (file (directory-files user-emacs-directory t "\\`[^.#].*\\.el\\'"))
  (unless (member (file-name-nondirectory file)
                  '("init.el" "early-init.el" "custom.el"
                    "package-quickstart.el"))
    (load (file-name-sans-extension file) nil t)))

;;; init.el ends here
