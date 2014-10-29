;;; package --- .emacs customizations

;;; Commentary:
;;; This is my (poorly organized) .emacs.  Roughly, it has four sections:
;;;
;;; * Package configuration;
;;; * Global key remappings;
;;; * Other misc settings;
;;; * Per-mode customizations.
;;;
;;; It's a constant work in progress.  There's probably a lot that
;;; sucks about it.

;;; Code:

(message "Loading ~/.emacs")

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun package-require (package)
  "Install a PACKAGE unless it is already installed.

Usage: (package-require 'package)"
  ;; try to activate the package with at least version 0.
  (package-activate package '(0))
  ;; try to just require the package. Maybe the user has it in the
  ;; local config
  (condition-case nil
      (require package)
    ;; if we cannot require it, it does not exist, yet. So install it.
    (error (package-install package))))

;; install use-package, which is *wonderful*
(package-require 'use-package)

;; solarized == teh business
(package-require 'solarized-theme)

;; set M-x compile to something handier.  this lets you run C-x C-m
;; C-m to compile
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

;; set C-c, c to comment-region and C-c, u to uncomment-region
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)

;; set C-c, p to point-to-register (current [p]oint to register) and
;; C-c, j to [j]ump-to-register
(global-set-key "\C-cp" 'point-to-register)
(global-set-key "\C-cj" 'jump-to-register)

;; set user-emacs-directory on older versions of emacs
(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory "~/.emacs.d"))
(if (file-exists-p user-emacs-directory)
    (add-to-list 'load-path user-emacs-directory))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(put 'scroll-left 'disabled nil)

;; window movement that doesn't suck
(use-package window-jump
  :ensure t
  :bind (("C-c w <up>" . window-jump-up)
         ("C-c w <down>" . window-jump-down)
         ("C-c w <left>" . window-jump-left)
         ("C-c w <right>" . window-jump-right)))

;; always show and trim trailing whitespace
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

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

;; set font
(defun font-exists (font-name)
  "Return t if FONT-NAME exists, nil otherwise."
  (if (functionp 'font-family-list)
      (member font-name (font-family-list))
    nil))

(if (font-exists "Andale Mono")
    (set-face-attribute 'default nil :font "Andale Mono"))

;; don't display the annoying splash screen
(setq inhibit-startup-message t)

;; disable text-fill by default
(auto-fill-mode nil)

;; miscellaneous other packages
(use-package autopair :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package fuzzy :ensure t)
(use-package google-this :ensure t)
(use-package markdown-mode :ensure t)
(use-package popup :ensure t)

;; load autocomplete
(use-package auto-complete-config
  :ensure auto-complete
  :diminish auto-complete-mode
  :config (progn
            (add-to-list 'ac-dictionary-directories
                         (concat user-emacs-directory "/ac-dict"))
            (ac-config-default)
            (define-key ac-completing-map [return] nil)
            (define-key ac-completing-map "\r" nil)))
(use-package auto-complete-nxml :ensure t)
(use-package auto-complete-rst :ensure t)

;; better grep
(use-package grep+ :ensure t)
(use-package grep-o-matic :ensure t)

;; automatically include licenses
(use-package legalese :ensure t)

;; http helpers
(use-package know-your-http-well :ensure t)
(use-package httprepl :ensure t)

;; create a sh-scratch buffer that's just like *scratch*, but with
;; the sh major mode
(with-current-buffer
    (generate-new-buffer "*sh-scratch*")
  (sh-mode))

;; text-mode settings
(add-hook 'text-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (flyspell-mode 1)))

;; perl mode settings
(add-hook 'perl-mode-hook
          '(lambda ()
             (local-set-key "\C-c\C-k" 'perlcritic)
             (require 'compile)
             (set (make-local-variable 'compile-command)
                  (concat "perl -c "
                          (file-name-nondirectory buffer-file-name)))))

(use-package perlcritic
  :ensure t
  :config (perlcritic-severity 4))

;; xml-mode settings
(setq auto-mode-alist (append '(("\\.xsd\\'" . xml-mode))
                              auto-mode-alist))
(setq auto-mode-alist (append '(("\\.rng\\'" . xml-mode))
                              auto-mode-alist))
(add-hook 'nxml-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq nxml-child-indent tab-width)
             (setq nxml-slash-auto-complete-flag t)
             (setq nxml-auto-insert-xml-declaration-flag t)
             (require 'compile)
             (set (make-local-variable 'compile-command)
                  (concat "xmllint --noout "
                          (file-name-nondirectory buffer-file-name)))))

(add-hook 'sgml-mode-hook 'turn-on-auto-fill)

;; org mode settings
(add-hook 'org-mode-hook
          '(lambda ()
             (org-indent-mode)
             (local-set-key "\C-cl" 'org-store-link)))

;; python mode settings
(use-package python
  :ensure python-mode
  :mode ("\\.wsgi" . python-mode))

(use-package sphinx-doc
  :ensure t
  :diminish sphinx-doc-mode)

;; nose test runner
(use-package nose
  :ensure t
  :config (progn
            (setq nose-global-name (executable-find "nosetests"))
            (define-key nose-mode-map "\C-cna" 'nosetests-all)
            (define-key nose-mode-map "\C-cnm" 'nosetests-module)
            (define-key nose-mode-map "\C-cn." 'nosetests-one)
            (define-key nose-mode-map "\C-cnc" 'nosetests-again)))

;; virtualenv stuff
(use-package virtualenvwrapper
  :ensure t
  :config (setq venv-location "~/venv/")
  :bind ("C-c v" . venv-workon))

;; pylookup config
(use-package pylookup
  :config (setq pylookup-db-file
                (concat user-emacs-directory "pylookup.db"))
  :bind ("C-c l" . pylookup-lookup-at-point))

;; jedi config
(use-package jedi
  :ensure t)

;; flycheck settings
(use-package flycheck
  :demand t
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :config (progn
            ;; better flycheck highlighting and faces
            (setq flycheck-highlighting-mode 'lines)
            (set-face-attribute 'flycheck-error nil :background "IndianRed1")
            (set-face-attribute 'flycheck-warning nil :background "gold1")
            (set-face-attribute 'flycheck-info nil :background "SkyBlue1")
            (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

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
  :init (progn
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

(add-hook 'python-mode-hook
          '(lambda ()
             (jedi:setup)
             (nose-mode t)
             (sphinx-doc-mode t)
             (setq python-fill-docstring-style 'pep-257-nn)
             (setq tab-width 4)
             (setq python-indent 4)
             (flycheck-select-checker 'pylint-pychecker)))

;; create a python-scratch buffer that's just like *scratch*, but with
;; the python major mode
(with-current-buffer
    (generate-new-buffer "*python-scratch*")
  (python-mode))

(use-package pymacs
  :ensure t
  :diminish ropemacs-mode
  :config (pymacs-load "ropemacs" "rope-")
  :bind (("C-<tab>" . rope-lucky-assist)
         ("M-." . rope-goto-definition)))

(add-hook 'rst-mode-hook '(lambda () (flyspell-mode 1)))

;; PHP mode settings
(add-hook 'php-mode-hook
          '(lambda ()
             (setq tab-width 4)
             (require 'compile)
             (set (make-local-variable 'compile-command)
                  (concat "php -l "
                          (file-name-nondirectory buffer-file-name)))))

;; go mode settings
(use-package go-mode :ensure t)
(add-hook 'go-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq standard-indent 2)
             (add-hook 'before-save-hook 'gofmt-before-save)))

;; Genshi template settings
(use-package genshi-mode
  :commands genshi-mode
  :mode (("\\.newtxt\\'" . genshi-mode)
         ("\\.genshi\\'" . genshi-mode)))

;; CSS mode settings
(use-package scss-mode :ensure t)
(add-hook 'css-mode-hook
          '(lambda ()
             (setq cssm-indent-level 4)
             (setq cssm-newline-before-closing-bracket t)
             (setq cssm-indent-function #'cssm-c-style-indenter)
             (setq cssm-mirror-mode nil)
             (hexcolor-add-to-font-lock)))

;; tabs suck, don't use them
(setq-default indent-tabs-mode nil)

;; search case-insensitive by default
(setq-default case-fold-search t)

;; enable the upcase-region command without confirmation
(put 'upcase-region 'disabled nil)

;; load RPM specfile mode
(use-package rpm-spec-mode
  :ensure t
  :mode "\\.spec\\'")

;; Always end a file with a newline
(setq require-final-newline t)

;; make scripts executable automatically
(package-require 'shebang)

;; make numbered backups in ~/.emacs.d/backups/
(use-package backup-each-save :ensure t)
(setq delete-old-versions t)
(setq version-control t)
(setq backup-by-copying t)
(let ((backups (concat user-emacs-directory
                       (convert-standard-filename "backups/"))))
  (setq backup-directory-alist (cons (cons "." backups) nil))

  ;; ensure that backup directory exists
  (if (not (file-exists-p backups))
      (if (file-regular-p backups)
          (message (concat backups " exists, but is not a directory"))
        (make-directory backups))))

;; set email address and fullname properly
(setq user-mail-address "chris.a.st.pierre@gmail.com")
(setq user-full-name "Chris St. Pierre")

;; Handle .gz files
(auto-compression-mode t)

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

;; better handling of duplicate buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; set initial window size
(setq default-frame-alist
      (append (list '(width  . 164)
                    '(height . 80)
                    '(font . "Andale Mono"))
              default-frame-alist))
;; we can't split the window initially when running as a daemon :(
;;(split-window-horizontally)

;; better mode line. much of this stolen from
;; https://github.com/lunaryorn/blog/blob/master/posts/make-your-emacs-mode-line-more-useful.md
(setq size-indication-mode t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq-default mode-line-position
              '((-3 "%p") (size-indication-mode ("/" (-4 "%I")))
                " "
                (line-number-mode
                 ("%l" (column-number-mode ":%c")))))
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

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; the toolbar and menu bar are wastes of valuable screen estate
(if (and (boundp 'tool-bar-mode) (fboundp 'tool-bar-mode))
    (tool-bar-mode -1))
(menu-bar-mode -1)

;; the blinking cursor is nothing but an annoyance
(blink-cursor-mode -1)

;; load local (site-specific) stuff
(if (file-exists-p "~/.emacs.local")
    (load "~/.emacs.local"))

;; goto the last change
(use-package goto-chg
  :ensure t
  :bind ("C-." . goto-last-change))

;; save my place in files
(require 'saveplace)
(setq-default save-place t)

;; load fpaste magic
(package-require 'fpaste)

;; graphviz mode settings
(use-package graphviz-dot-mode
  :ensure t
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\'" . graphviz-dot-mode)))

(add-hook 'graphviz-dot-mode-hook
          '(lambda ()
             (setq tab-width 4)
             (setq graphviz-dot-indent-width 4)
             (setq graphviz-dot-auto-indent-on-newline nil)
             (setq graphviz-dot-auto-indent-on-braces nil)
             (setq graphviz-dot-auto-indent-on-semi nil)))

;; yaml-mode settings
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

;; json-mode settings
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")
(add-hook 'json-mode-hook
          '(setq js-indent-level 2))

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

;; selinux .te file settings
(use-package selinux-te-mode
  :commands selinux-te-mode
  :mode "\\.te\\'")

;;; .emacs ends here
