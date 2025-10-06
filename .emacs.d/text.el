(add-hook 'text-mode-hook
          #'(lambda ()
              (turn-on-auto-fill)
              (flyspell-mode 1)))

;; create a text-scratch buffer that's just like *scratch*, but with
;; the text major mode
(with-current-buffer
    (generate-new-buffer "*text-scratch*")
  (text-mode))
