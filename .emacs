(princ "Loading ~/.emacs")

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(auto-complete
                      autopair
                      fuzzy
                      go-mode
                      google-this
                      graphviz-dot-mode
                      json-mode
                      markdown-mode
                      popup
                      pymacs
                      python-mode
                      rpm-spec-mode
                      yaml-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; don't display the annoying splash screen
(setq inhibit-splash-screen t)

;; open Alpine temp files (pico.NNNNN) in text-mode
(setq auto-mode-alist (append '(("pico\\." . text-mode))
                              auto-mode-alist))

;; enable text-fill by default in text modes, disable in other modes
(auto-fill-mode nil)
(setq comment-auto-fill-only-comments t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'tbemail-mode-hook 'turn-on-auto-fill)

;; set M-x compile to something handier.  this lets you run C-x C-m
;; C-m to compile
(global-set-key "\C-x\C-m" 'compile)

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

;; set hideshow mode keys to something easier
(global-set-key "\C-cs" 'hs-show-block)
(global-set-key "\C-ch" 'hs-hide-block)
(global-set-key "\C-ct" 'hs-toggle-hiding)
(global-set-key "\C-cS" 'hs-show-all)
(global-set-key "\C-cH" 'hs-hide-all)

;; set user-emacs-directory on older versions of emacs
(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory "~/.emacs.d"))
(if (file-exists-p user-emacs-directory)
    (add-to-list 'load-path user-emacs-directory))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(put 'scroll-left 'disabled nil)

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
  (interactive)
  (font-lock-add-keywords nil hexcolor-keywords t))

;; load autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/Users/stpierre/.emacs.d/ac-dict")
(ac-config-default)
(define-key ac-completing-map [return] nil)
(define-key ac-completing-map "\r" nil)

;; perl mode settings
(add-hook 'perl-mode-hook
          '(lambda ()
             (local-set-key "\C-c\C-k" 'perlcritic)
             (require 'compile)
             (hs-minor-mode)
             (set (make-local-variable 'compile-command)
                  (concat "perl -c "
                          (file-name-nondirectory buffer-file-name)))))

(eval-after-load "perlcritic"
  '(perlcritic-severity 4))

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
(setq pylookup-db-file (concat user-emacs-directory "pylookup.db"))
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)
(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)
(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))

(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "<C-tab>") 'rope-lucky-assist)
             (setq python-fill-docstring-style 'pep-257-nn)
             (setq tab-width 4)
             (setq python-indent 4)
             (hs-minor-mode)
             (flycheck-select-checker 'pylint-pychecker)
             (unless (assoc 'python-mode hs-special-modes-alist)
               (setq hs-special-modes-alist
                     (cons (list 'python-mode
                                 "^\\s-*def\\>" nil "#"
                                 (lambda (arg)
                                   (py-end-of-def-or-class)
                                   (skip-chars-backward " \t\n"))
                                 nil)
                           hs-special-modes-alist)))))

;; create a python-scratch buffer that's just like *scratch*, but with
;; the python major mode
(with-current-buffer
    (generate-new-buffer "*python-scratch*")
  (python-mode))

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")


;; PHP mode settings
(add-hook 'php-mode-hook
          '(lambda ()
             (setq tab-width 4)
             (require 'compile)
             (hs-minor-mode)
             (set (make-local-variable 'compile-command)
                  (concat "php -l "
                          (file-name-nondirectory buffer-file-name)))))

;; go mode settings
(add-hook 'go-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq standard-indent 2)
             (add-hook 'before-save-hook 'gofmt-before-save)))

;; Genshi template settings
(autoload 'genshi-mode
  (concat user-emacs-directory "genshi-mode.el")
  "Genshi mode." t)
(setq auto-mode-alist (append '(("\\.newtxt\\'" . genshi-mode))
                              auto-mode-alist))
(setq auto-mode-alist (append '(("\\.genshi\\'" . genshi-mode))
                              auto-mode-alist))

;; CSS mode settings
(add-hook 'css-mode-hook
          '(lambda ()
             (setq cssm-indent-level 4)
             (setq cssm-newline-before-closing-bracket t)
             (setq cssm-indent-function #'cssm-c-style-indenter)
             (setq cssm-mirror-mode nil)
             (hexcolor-add-to-font-loc)))

;; tabs suck, don't use them
(setq-default indent-tabs-mode nil)

;; search case-insensitive by default
(setq-default case-fold-search t)

;; enable the upcase-region command without confirmation
(put 'upcase-region 'disabled nil)

;; load RPM specfile mode
(autoload 'rpm-spec-mode
  (concat user-emacs-directory "rpm-spec-mode.el")
  "RPM spec mode." t)
(setq auto-mode-alist (append '(("\\.spec" . rpm-spec-mode)) auto-mode-alist))

;; Always end a file with a newline
(setq require-final-newline t)

;; make scripts executable automatically
(require 'shebang)

;; make numbered backups in ~/.emacs.d/backups/
(setq backups
      (concat user-emacs-directory (convert-standard-filename "backups/")))
(setq delete-old-versions t)
(setq version-control t)
(setq backup-by-copying t)
(setq backup-directory-alist (cons (cons "." backups) nil))

;; ensure that backup directory exists
(if (not (file-exists-p backups))
    (if (file-regular-p backups)
        (princ (concat backups " exists, but is not a directory"))
      (make-directory backups)))

;; set email address and fullname properly
(defun user-mail-address () "chris.a.st.pierre@gmail.com")
(defun user-full-name () "Chris St. Pierre")

;; Handle .gz files
(auto-compression-mode t)

;; define unfill commands (http://www.emacswiki.org/emacs/UnfillParagraph)
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(define-key global-map "\M-Q" 'unfill-paragraph)
(define-key global-map "\M-\C-q" 'unfill-region)

;; set initial window size
(setq default-frame-alist
      (append (list '(width  . 164)
                    '(height . 80)
                    '(font . "Andale Mono"))
              default-frame-alist))
;; we can't split the window initially when running as a daemon :(
;;(split-window-horizontally)

;; better mode line
(line-number-mode t)
(column-number-mode t)
(if (and (boundp 'size-indication-mode)
         (fboundp 'size-indication-mode))
    (size-indication-mode t))

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

;; load fpaste magic
(require 'fpaste)

;; graphviz mode settings
(autoload 'graphviz-dot-mode
  (concat user-emacs-directory "graphviz-dot-mode.el")
  "Graphviz mode." t)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))

(add-hook 'graphviz-dot-mode-hook
          '(lambda ()
             (setq tab-width 4)
             (setq graphviz-dot-indent-width 4)
             (setq graphviz-dot-auto-indent-on-newline nil)
             (setq graphviz-dot-auto-indent-on-braces nil)
             (setq graphviz-dot-auto-indent-on-semi nil)))

;; yaml-mode settings
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; json-mode settings
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'json-mode-hook
          '(setq js-indent-level 2))

;; flycheck settings
(add-hook 'after-init-hook #'global-flycheck-mode)

;; better flycheck highlighting and faces
(eval-after-load "flycheck"
  '(progn
     (setq flycheck-highlighting-mode 'lines)
     (set-face-attribute 'flycheck-error nil :background "IndianRed1")
     (set-face-attribute 'flycheck-warning nil :background "gold1")
     (set-face-attribute 'flycheck-info nil :background "SkyBlue1")
     (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

     (define-key flycheck-mode-map flycheck-keymap-prefix nil)
     (setq flycheck-keymap-prefix (kbd "C-c e"))
     (define-key flycheck-mode-map flycheck-keymap-prefix
       flycheck-command-map)
     (define-key flycheck-mode-map (kbd "C-c e s")
       'flycheck-display-err-in-minibuffer)
     (define-key flycheck-mode-map (kbd "C-c e d")
       'flycheck-disable-pylint-on-line)
     (define-key flycheck-mode-map (kbd "C-c e q") 'disable-qa-on-line)
     (define-key flycheck-mode-map (kbd "C-c e v") 'disable-cover-on-line)))

;; use our own python checker
(flycheck-define-checker pylint-pychecker
  "Python checker that uses pylint and flake8 and respects local configs."
  :command ("~/bin/pychecker.py" "--original" source-original source-inplace)
  :error-patterns
  ((warning line-start (1+ not-newline) ":" line ": "
            (message "[" (or "C" "W") (1+ not-newline)) line-end)
   (info line-start (1+ not-newline) ":" line ": "
         (message (or "No test coverage" (group "[" (or "I" "R")))
                  (0+ not-newline)) line-end)
   (error line-start (1+ not-newline) ":" line ": "
          (message (1+ not-newline)) line-end))
  :modes (python-mode))

;; keep state for flycheck-display-err-in-minibuffer so multiple
;; invocations cycle through the errors for a given line
(setq flycheck-last-error-idx 0)
(setq flycheck-last-error-point 0)

(defun flycheck-errors-at-point ()
  (let ((old-point (point)))
    (back-to-indentation)
    (let ((errors (flycheck-overlay-errors-at (point))))
      (goto-char old-point)
      errors)))

(defun flycheck-display-err-in-minibuffer ()
  (interactive)
  (let ((messages (delq nil (mapcar #'flycheck-error-message
                                    (flycheck-errors-at-point)))))
    (progn
      (if (= (point) flycheck-last-error-point)
          (setq flycheck-last-error-idx (1+ flycheck-last-error-idx))
        (setq flycheck-last-error-idx 0))
      (setq flycheck-last-error-point (point))
      (setq err (nth flycheck-last-error-idx messages))
      (if err nil
        (progn
          ;; went past the end of the error list -- start over
          (setq flycheck-last-error-idx -1)
          (setq err (car messages))))
      (princ err))))

(defun flycheck-error-pylint-name (err)
  (let ((message (flycheck-error-message err)))
    (if (string-match "^\\[.*(\\([^)]*\\))" message)
        (match-string 1 message)
      nil)))

(defun flycheck-disable-pylint-on-line ()
  (interactive)
  (let ((errors (delq nil (mapcar 'flycheck-error-pylint-name
                                  (flycheck-errors-at-point)))))
    (end-of-line)
    (insert
     (format "  # pylint: disable=%s"
             (mapconcat 'identity errors ",")))))

(defun disable-qa-on-line ()
  (interactive)
  (end-of-line)
  (insert "  # noqa"))

(defun disable-cover-on-line ()
  (interactive)
  (end-of-line)
  (insert "  # pragma: nocover"))

;; selinux .te file settings
(autoload 'selinux-te-mode
  (concat user-emacs-directory "selinux-mode.el")
  "SELinux TE mode." t)
(setq auto-mode-alist (append '(("\\.te\\'" . selinux-te-mode))
                              auto-mode-alist))

;; set font
(defun font-exists (font-name)
  (if (functionp 'font-family-list)
      (member font-name (font-family-list))
    nil))

(if (font-exists "Andale Mono")
    (set-face-attribute 'default nil :font "Andale Mono"))

;;; .emacs ends here
