;;; genshi-mode.el -- Genshi template mode for Emacs

;;; Commentary:
;;; based on two-mode-mode by David N. Welton <davidw@dedasys.com>

;;; Code:

(defgroup genshi nil
  "Genshi template editing mode"
  :prefix "genshi-"
  :group 'languages)

(defgroup genshi-faces nil
  "Font lock faces for `genshi-mode'."
  :group 'genshi
  :group 'faces)


;; Genshi-mode hook
(defvar genshi-mode-hook nil
  "*Hook called by `genshi'.")
(setq genshi-mode-hook nil)

;;--------------------------------------------------
;; Mode-switching magic

(defvar default-mode (list "Conf[Unix]" 'conf-unix-mode))
(defvar second-modes (list
                      (list "Python" "{% python" "%}" 'python-mode)))

(defvar genshi-update 0)
(defvar genshi-mode-idle-timer nil)
(defvar genshi-bool nil)
(defvar genshi-mode-delay (/ (float 1) (float 8)))

;; Mode switching hook
(defvar genshi-switch-hook nil
  "*Hook called upon mode switching.")
(setq genshi-switch-hook nil)

(defun genshi-mode-setup ()
  (add-hook 'post-command-hook 'genshi-mode-need-update nil t)
  (make-local-variable 'minor-mode-alist)
  (make-local-variable 'genshi-bool)
  (setq genshi-bool t)
  (when genshi-mode-idle-timer
    (cancel-timer genshi-mode-idle-timer))
  (setq genshi-mode-idle-timer
	(run-with-idle-timer genshi-mode-delay t
			     'genshi-mode-update-mode))
  (or (assq 'genshi-bool minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(genshi-bool " genshi") minor-mode-alist))))

(defun genshi-mode-need-update ()
  (setq genshi-update 1))

(defun genshi-change-mode (to-mode func)
  (if (string= to-mode mode-name)
      t
    (progn
      (funcall func)
      ;; After the mode was set, we reread the "Local Variables" section.
      ;; We do need this for example in SGML-mode if "sgml-parent-document"
      ;; was set, or otherwise it will be reset to nil when sgml-mode is left.
      (hack-local-variables)

      (genshi-mode-setup)
      (if genshi-switch-hook
	  (run-hooks 'genshi-switch-hook))
      (if (eq font-lock-mode t)
	  (font-lock-fontify-buffer))
      (turn-on-font-lock))))

(defun genshi-mode-update-mode ()
  (when (and genshi-bool genshi-update)
    (setq genshi-update 0)
    (let ((mode-list second-modes)
	  (flag 0))
      (while mode-list
	(let ((mode (car mode-list))
	      (lm -1)
	      (rm -1))
	  (save-excursion
	    (if (search-backward (cadr mode) nil t)
		(setq lm (point))
	      (setq lm -1)))
	  (save-excursion
	    (if (search-backward (car (cddr mode)) nil t)
		(setq rm (point))
	      (setq rm -1)))
	  (if (and (not (and (= lm -1) (= rm -1))) (>= lm rm))
	      (progn
		(setq flag 1)
		(setq mode-list '())
		(genshi-change-mode (car mode) (car (cdr (cddr mode)))))))
	(setq mode-list (cdr mode-list)))
      (if (= flag 0)
	  (genshi-change-mode (car default-mode) (cadr default-mode))))))

(defun genshi-mode ()
  "Turn on genshi-mode."
  (interactive)
  (funcall (cadr default-mode))
  (genshi-mode-setup)
  (if genshi-mode-hook
      (run-hooks 'genshi-mode-hook)))

;;--------------------------------------------------
;; Font lock stuff

(defconst genshi-font-lock-keywords
  (list
   '("\\<\\(choose\\|def\\|end\\|for\\|i\\(?:nclude\\|[fn]\\)\\|otherwise\\|python\\|w\\(?:hen\\|ith\\)\\)\\>" . font-lock-builtin-face)
   '("\\({%\\) \\(i\\(?:nclude\\|f)|when\\) .* \\(%}\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-builtin-face)
     (3 font-lock-keyword-face))
   '("\\({%\\) \\(choose|end|otherwise\\) \\(%}\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-builtin-face)
     (3 font-lock-keyword-face))
   '("\\({%\\) \\(for\\) .* \\(in\\) .* \\(%}\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-builtin-face)
     (3 font-lock-builtin-face)
     (4 font-lock-keyword-face))
   '("\\({%\\) \\(def\\) \\([A-Za-z0-9_]+\\)\\((.*)\\)? \\(%}\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-builtin-face)
     (3 font-lock-function-name-face)
     (5 font-lock-keyword-face))
   '("\\({%\\) \\(with\\) \\([A-Za-z0-9_]+\\)=\\(.*\\) \\(%}\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-builtin-face)
     (3 font-lock-variable-name-face)
     (5 font-lock-keyword-face))
   '("\\({#\\).*\\(#}\\)"
     (1 font-lock-comment-delimiter-face)
     (2 font-lock-comment-face)
     (3 font-lock-comment-delimiter-face))
   '("\\({%\\) \\(python\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-builtin-face))
   '("{%\\>" 0 font-lock-keyword-face)
   '("\\<%}" 0 font-lock-keyword-face)
   '("\\(${\\).*\\(}\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-keyword-face))
   )
  )

(provide 'genshi-mode)

;;; genshi-mode.el ends here
