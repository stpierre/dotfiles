;;; package --- .emacs customizations

;;; Commentary:
;;; Initial emacs startup bits. Most stuff should be organized into
;;; other files.

;;; Code:

(require 'package)
(setq package-quickstart t)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)
(add-to-list
 'package-archives '("elpa" . "https://elpa.gnu.org/packages/")
 t)

(when (not package-archive-contents)
  (package-refresh-contents))

(require 'cl-lib)
(require 'subword)

;; install use-package, which is *wonderful*
(require 'use-package)

(require 'saveplace)
(setq save-place t)

(setq
 auto-save-default nil
 echo-keystrokes 0.1
 imenu-auto-rescan t
 imenu-flatten "annotation"
 initial-major-mode 'fundamental-mode
 initial-scratch-message ""
 make-backup-files nil
 require-final-newline t
 show-trailing-whitespace t)

(setq-default
 indent-tabs-mode nil
 case-fold-search t
 fill-column 79)

;; set user-emacs-directory on older versions of emacs
(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory "~/.emacs.d"))

;; add a place to put custom packages that aren't in ELPA/MELPA
(if (file-exists-p user-emacs-directory)
    (add-to-list
     'load-path (concat user-emacs-directory "/packages")))

(auto-fill-mode nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; set email address and fullname properly
(setq user-mail-address "chris.a.st.pierre@gmail.com")
(setq user-full-name "Chris St. Pierre")

;; Handle .gz files
(auto-compression-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(defun sort-lines-nocase ()
  "Sort lines case-insensitively."
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(require 'shebang)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, quit, and shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(use-package dumb-jump :ensure t)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(use-package corfu
  :ensure t
  :hook ((prog-mode . corfu-mode)))

(use-package
 emacs
 :ensure t
 :custom (tab-always-indent 'complete)
 (read-extended-command-predicate
  #'command-completion-default-include-p))

(mapc
 'load
 (cl-remove-if
  (lambda (p)
    (string= (file-name-nondirectory p) "init.el"))
  (file-expand-wildcards "~/.emacs.d/*.el")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
