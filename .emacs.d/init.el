;;; package --- .emacs customizations

;;; Commentary:
;;; Initial emacs startup bits. Most stuff should be organized into
;;; other files.

;;; Code:

(require 'package)
(setq package-quickstart t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents))

(require 'cl-lib)
(require 'subword)

;; install use-package, which is *wonderful*
(require 'use-package)

(require 'uniquify)
(require 'saveplace)

(setq
 auto-save-default nil
 make-backup-files nil
 echo-keystrokes 0.1
 enable-local-variables :all
 initial-major-mode 'fundamental-mode
 initial-scratch-message ""
 uniquify-buffer-name-style 'post-forward-angle-brackets
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*"
 require-final-newline t
 show-trailing-whitespace t)

(setq-default
 indent-tabs-mode nil
 case-fold-search t
 save-place t)

;; set user-emacs-directory on older versions of emacs
(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory "~/.emacs.d"))

;; add a place to put custom packages that aren't in ELPA/MELPA
(if (file-exists-p user-emacs-directory)
    (add-to-list 'load-path (concat user-emacs-directory "/packages")))

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

(use-package find-file-in-repository
  :bind (("C-x f" . find-file-in-repository)))

(mapc 'load
      (cl-remove-if (lambda (p) (string= (file-name-nondirectory p) "init.el"))
                    (file-expand-wildcards "~/.emacs.d/*.el")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(kotlin-mode python-black poetry kotlin-ts-mode yaml-mode highlight-indent-guides solarized-theme terraform-mode sqlformat rpm-spec-mode window-jump sphinx-doc scss-mode pyvenv python-mode pyenv-mode py-isort markdown-mode legalese know-your-http-well just-mode json-mode jedi httprepl groovy-mode graphviz-dot-mode gotest go-mode flycheck-color-mode-line dumb-jump dockerfile-mode backup-each-save auto-complete-rst auto-complete-nxml)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
