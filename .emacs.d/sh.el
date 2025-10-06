;; create a sh-scratch buffer that's just like *scratch*, but with
;; the sh major mode
(with-current-buffer
    (generate-new-buffer "*sh-scratch*")
  (sh-mode))

(add-hook 'sh-mode-hook 'eglot-ensure)

;;  (add-to-list 'eglot-server-programs '((sh-mode bash-ts-mode) . ("bash-language-server" "start")))
