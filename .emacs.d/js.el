(defvar hexcolor-keywords
  '(("#[[:xdigit:]]\\{3,6\\}"
     (0 (let ((color (match-string-no-properties 0)))
          (if (or (= (length color) 4)
                  (= (length color) 7))
              (put-text-property
               (match-beginning 0)
               (match-end 0)
               'face (list :background
                           (match-string-no-properties 0)
                           :foreground
                           (if (>= (apply '+ (x-color-values
                                              (match-string-no-properties 0)))
                                   (* (apply '+ (x-color-values "white")) .6))
                               "black" ;; light bg, dark text
                             "white" ;; dark bg, light text
                             )))))
        append))))

(defun hexcolor-add-to-font-lock ()
  "Make hex color strings display in the color they describe."
  (interactive)
  (font-lock-add-keywords nil hexcolor-keywords t))

;; javascript-mode settings
(add-hook 'javascript-mode-hook
          #'(lambda ()
              (setq tab-width 2)
              (setq js-indent-level 2)))

(use-package scss-mode
  :ensure t
  :config (setq
           css-indent-offset 2
           cssm-newline-before-closing-bracket t
           cssm-indent-function #'cssm-c-style-indenter
           cssm-mirror-mode nil)
  :hook (css-mode . hexcolor-add-to-font-lock))
