;; solarized == teh business
(use-package solarized-theme
  :ensure t
  :init (load-theme 'solarized-light t))

(put 'scroll-left 'disabled nil)

;; set font
(defun font-exists (font-name)
  "Return t if FONT-NAME exists, nil otherwise."
  (if (functionp 'font-family-list)
      (member font-name (font-family-list))
    nil))

;; TODO: really need to make this autodiscover the best font to use,
;; but I ran into strange issues with that. on OS X this (and the
;; other references below) should be Andale Mono
(if (font-exists "liberation mono")
    (set-face-attribute 'default nil :font "liberation mono" :height 90))

;; set initial window size
(setq default-frame-alist
      (append (list '(width  . 164)
                    '(height . 80)
                    ;;                    '(font . "liberation mono-9")
                    )
              default-frame-alist))

;; (defun hidpi-font-on ()
;;   "Convenience method for setting font size on HiDPI monitors."
;;   (interactive)
;;   (set-frame-font "liberation mono-11"))

;; (defun hidpi-font-off ()
;;   "Convenience method for setting font size on HiDPI monitors."
;;   (interactive)
;;   (set-frame-font "liberation mono-9"))

(setq inhibit-startup-message t
      size-indication-mode t
      line-number-mode t
      column-number-mode t)

(setq-default mode-line-position
              '((-3 "%p") (size-indication-mode ("/" (-4 "%I")))
                " "
                (line-number-mode
                 ("%l" (column-number-mode ":%c")))))

;; better mode line. much of this stolen from
;; https://github.com/lunaryorn/blog/blob/master/posts/make-your-emacs-mode-line-more-useful.md
(defvar lunaryorn-vc-mode-line
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (substring vc-mode (+ (length backend) 2))))
         face font-lock-variable-name-face))
  "Mode line format for VC Mode.")
(put 'lunaryorn-vc-mode-line 'risky-local-variable t)
(setq-default mode-line-format
              (list ""
                    'mode-line-modified " "
                    'mode-line-buffer-identification
                    '(vc-mode lunaryorn-vc-mode-line) " "
                    'mode-line-position " "
                    'mode-line-modes
                    'mode-line-misc-info))

;; the toolbar and menu bar are wastes of valuable screen estate
(tool-bar-mode -1)
(menu-bar-mode -1)

;; the blinking cursor is nothing but an annoyance
(blink-cursor-mode -1)

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode))
