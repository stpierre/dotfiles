;; set M-x compile to something handier.
(global-set-key "\C-x\C-m" 'compile)

;; better buffer listing
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; better searching
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; set some navigation keys
(global-set-key "\M-gg" 'goto-line)
(global-set-key "\M-[" 'backward-paragraph)
(global-set-key "\M-]" 'forward-paragraph)

;; set C-c, p to point-to-register (current [p]oint to register) and
;; C-c, j to [j]ump-to-register
(global-set-key "\C-cp" 'point-to-register)
(global-set-key "\C-cj" 'jump-to-register)

;; window movement that doesn't suck
(use-package window-jump
  :ensure t
  :bind (("C-c w <up>" . window-jump-up)
         ("C-c w <down>" . window-jump-down)
         ("C-c w <left>" . window-jump-left)
         ("C-c w <right>" . window-jump-right)))

(with-eval-after-load 'prog-mode
  (define-key prog-mode-map (kbd "C-c c") 'comment-region)
  (define-key prog-mode-map (kbd "C-c u") 'uncomment-region))

;; define unfill commands (http://www.emacswiki.org/emacs/UnfillParagraph)
(defun unfill-paragraph ()
  "Make multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  "Unfill all paragraphs in region."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(define-key global-map "\M-Q" 'unfill-paragraph)
(define-key global-map "\M-\C-q" 'unfill-region)
