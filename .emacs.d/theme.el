;; solarized == teh business
(use-package solarized-theme
  :ensure t
  :init (load-theme 'solarized-light t))

(put 'scroll-left 'disabled nil)

;; set font
(defun font-exists-p (font-name)
  "Return t if FONT-NAME exists, nil otherwise."
  (if (functionp 'font-family-list)
      (> (length (member font-name (font-family-list))) 0)
    nil))

(defun default-font ()
  "Discover the default font to use."
  (if (font-exists-p "liberation mono")
      "liberation mono"
    (if (font-exists-p "Monaco")
        "Monaco")))
;; todo add an else here in case neither exists

(defun default-font-size ()
  "Discover the default font size to use."
  (if (eq (default-font) "liberation mono")
      9 ;; linux, adjust for hidpi monitor
    11)) ;; mac os

(defun hidpi-font-on ()
  "Convenience method for setting font size on HiDPI monitors."
  (interactive)
  (set-frame-font (concat (default-font) "-11")))

(defun hidpi-font-off ()
  "Convenience method for setting font size on HiDPI monitors."
  (interactive)
  (set-frame-font (concat (default-font) "-9")))

;;(set-face-attribute 'default nil
;;                    :family (default-font)
;;                    :height (* 10 (default-font-size)))

;; set initial window size
(setq default-frame-alist
     '((width  . 164)
       (height . 80)))

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
