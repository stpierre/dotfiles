(use-package python
  :ensure python-mode
  :mode ("\\.wsgi" . python-mode)
  :config (setq python-fill-docstring-style 'pep-257-nn
                tab-width 4
                python-indent 4)
  :hook (python-mode . (lambda ()
                         (pyenv-mode)
                         (jedi:setup)
                         (sphinx-doc-mode t)
                         (python-black-on-save-mode)
                         (add-hook 'before-save-hook 'py-isort-before-save)
                         (flycheck-select-checker 'pylint-pychecker)
	                 )))

(use-package python-black
  :demand t
  :after python
  :config (setq python-black-extra-args '("--line-length=79")))

(use-package pyenv-mode
  :ensure t
  :after python)

(use-package sphinx-doc
  :ensure t
  :diminish sphinx-doc-mode)

(use-package pyvenv
  :ensure t
  :bind ("C-c v" . pyvenv-activate))

(use-package pylookup
  :config (setq pylookup-db-file
                (concat user-emacs-directory "pylookup.db"))
  :bind ("C-c l" . pylookup-lookup-at-point))

(use-package jedi
  :ensure t)

(use-package py-isort
  :ensure t)

;; flycheck settings
(use-package flycheck
  :demand t
  :hook (after-init . global-flycheck-mode)
  :hook (flycheck-mode . flycheck-color-mode-line-mode)
  :config (progn
            ;; better flycheck highlighting and faces
            (setq flycheck-highlighting-mode 'lines)
            (set-face-attribute 'flycheck-error nil :background "IndianRed1")
            (set-face-attribute 'flycheck-warning nil :background "gold1")
            (set-face-attribute 'flycheck-info nil :background "SkyBlue1")

            ;; map flycheck commands to C-c, e, * instead of C-c, !, *
            (define-key flycheck-mode-map flycheck-keymap-prefix nil)
            (setq flycheck-keymap-prefix (kbd "C-c e"))
            (define-key flycheck-mode-map flycheck-keymap-prefix
              flycheck-command-map)
            (define-key flycheck-mode-map (kbd "C-c e s")
              'flycheck-display-err-in-minibuffer)
            (define-key flycheck-mode-map (kbd "C-c e d")
              'flycheck-disable-pylint-on-line)
            (define-key flycheck-mode-map (kbd "C-c e q")
              'disable-qa-on-line)
            (define-key flycheck-mode-map (kbd "C-c e v")
              'disable-cover-on-line)

            ;; use our own python checker
            (flycheck-define-checker pylint-pychecker
              "pylint/flake8 checker that respects local configs."
              :command ("~/bin/pychecker.py"
                        "--original" source-original
                        source-inplace)
              :error-patterns
              ((warning line-start (1+ not-newline) ":" line ": "
                        (message "[" (or "C" "W") (1+ not-newline)) line-end)
               (info line-start (1+ not-newline) ":" line ": "
                     (message (or "No test coverage" (group "[" (or "I" "R")))
                              (0+ not-newline)) line-end)
               (error line-start (1+ not-newline) ":" line ": "
                      (message (1+ not-newline)) line-end))
              :modes (python-mode))

            ;; functions to display and disable errors on demand

            (defun flycheck-errors-at-point ()
              "Convenience method to list all flycheck errors at point.

             This actually gets errors from the first non-space
             character, which is more reliable; with whole-line
             highlighting, flycheck actually only highlights
             non-space characters, so if your point is after the
             end of the line, it will report no errors."
              (let ((old-point (point)))
                (back-to-indentation)
                (let ((errors (flycheck-overlay-errors-at (point))))
                  (goto-char old-point)
                  errors)))

            (defun flycheck-error-pylint-name (err)
              "Get the symbolic name of the pylint error ERR found by flycheck."
              (let ((message (flycheck-error-message err)))
                (if (string-match "^\\[.*(\\([^)]*\\))" message)
                    (match-string 1 message)
                  nil)))

            (defun flycheck-disable-pylint-on-line ()
              "Disable pylint for all flycheck errors in the line at point."
              (interactive)
              (let ((errors (delq nil (mapcar 'flycheck-error-pylint-name
                                              (flycheck-errors-at-point)))))
                (end-of-line)
                (insert
                 (format "  # pylint: disable=%s"
                         (mapconcat 'identity errors ",")))))))

(use-package flycheck-color-mode-line
  :ensure t
  :config (progn
            (set-face-attribute 'flycheck-color-mode-line-error-face
                                nil
                                :background "#ff6e64"
                                :foreground "#002b36")
            (set-face-attribute 'flycheck-color-mode-line-warning-face
                                nil
                                :background "#deb542"
                                :foreground "#002b36")
            (set-face-attribute 'flycheck-color-mode-line-info-face
                                nil
                                :background "#69b7f0"
                                :foreground "#002b36")))

(defun py-isort-disable-on-save ()
  "Disable auto-isort for the current buffer."
  (interactive)
  (remove-hook 'before-save-hook 'py-isort-before-save))

;; create a python-scratch buffer that's just like *scratch*, but with
;; the python major mode
(with-current-buffer
    (generate-new-buffer "*python-scratch*")
  (python-mode))

(defun disable-qa-on-line ()
  "Disable python QA on the current line."
  (interactive)
  (end-of-line)
  (insert "  # noqa"))

(defun disable-cover-on-line ()
  "Disable checking for unit test coverage on the current line."
  (interactive)
  (end-of-line)
  (insert "  # pragma: nocover"))
