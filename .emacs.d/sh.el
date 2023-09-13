;; create a sh-scratch buffer that's just like *scratch*, but with
;; the sh major mode
(with-current-buffer
    (generate-new-buffer "*sh-scratch*")
  (sh-mode))
