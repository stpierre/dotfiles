;;; bindings.el --- Global key bindings -*- lexical-binding: t -*-

;;; Commentary:
;;; Global key bindings and the commands that only exist to be bound.

;;; Code:

;; better buffer listing
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; better searching
(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-r") #'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") #'isearch-forward)
(global-set-key (kbd "C-M-r") #'isearch-backward)

;; set C-c, p to point-to-register (current [p]oint to register) and
;; C-c, j to [j]ump-to-register
(global-set-key (kbd "C-c p") #'point-to-register)
(global-set-key (kbd "C-c j") #'jump-to-register)

(global-set-key (kbd "C-c c") #'comment-region)
(global-set-key (kbd "C-c u") #'uncomment-region)

;; window movement that doesn't suck
(global-set-key (kbd "C-c w <up>") #'windmove-up)
(global-set-key (kbd "C-c w <down>") #'windmove-down)
(global-set-key (kbd "C-c w <left>") #'windmove-left)
(global-set-key (kbd "C-c w <right>") #'windmove-right)

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

(global-set-key (kbd "M-Q") #'unfill-paragraph)
(global-set-key (kbd "C-M-q") #'unfill-region)

;;; bindings.el ends here
