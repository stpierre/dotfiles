(add-hook 'text-mode-hook
          #'(lambda ()
              (turn-on-auto-fill)
              (flyspell-mode 1)))

;; create a text-scratch buffer that's just like *scratch*, but with
;; the text major mode
(with-current-buffer
    (generate-new-buffer "*text-scratch*")
  (text-mode))

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
