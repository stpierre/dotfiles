;;; early-init.el --- Settings needed before package activation -*- lexical-binding: t -*-

;;; Commentary:
;;; Loaded before packages are activated and before the first frame
;;; is created.

;;; Code:

(setq package-quickstart t
      load-prefer-newer t)

;; defer garbage collection until startup is done
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))

;; never build UI elements that we're just going to turn off
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;; early-init.el ends here
