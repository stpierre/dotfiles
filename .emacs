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
                      flymake
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

;; function to run pre-save checks
(defun presave-process-check (command &rest args)
  "Call an external process on the contents of the buffer"
  (setq buffer (get-buffer-create (concat "*" command "*")))
  (save-excursion
    (set-buffer buffer)
    (delete-region (point-min) (point-max)))
  (if (/= 0 (apply 'call-process-region
                   (point-min) (point-max)
                   command nil buffer t args))
      (pop-to-buffer buffer)))

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
             (setq show-trailing-whitespace t)
             (setq tab-width 2)
             (setq nxml-child-indent tab-width)
             (setq nxml-slash-auto-complete-flag t)
             (setq nxml-auto-insert-xml-declaration-flag t)
             (require 'compile)
             (set (make-local-variable 'compile-command)
                  (concat "xmllint --noout "
                          (file-name-nondirectory buffer-file-name)))
             (add-hook 'local-write-file-hooks
                       '(lambda()
                          (delete-trailing-whitespace)
                          (presave-process-check "xmllint" "--noout" "-")))))

(add-hook 'sgml-mode-hook 'turn-on-auto-fill)

;; org mode settings
(add-hook 'org-mode-hook
          '(lambda ()
             (org-indent-mode)
             (local-set-key "\C-cl" 'org-store-link)))

;; markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

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
             (add-hook 'before-save-hook 'delete-trailing-whitespace)
             (hs-minor-mode)
             (unless (assoc 'python-mode hs-special-modes-alist)
               (setq hs-special-modes-alist
                     (cons (list 'python-mode
                                 "^\\s-*def\\>" nil "#"
                                 (lambda (arg)
                                   (py-end-of-def-or-class)
                                   (skip-chars-backward " \t\n"))
                                 nil)
                           hs-special-modes-alist)))))
(add-hook 'python-mode-hook 'jedi:setup)

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
; we can't split the window initially when running as a daemon :(
;(split-window-horizontally)

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

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; create a python-scratch buffer that's just like *scratch*, but with
;; the python major mode
(with-current-buffer
    (generate-new-buffer "*python-scratch*")
  (python-mode))

(put 'scroll-left 'disabled nil)

;; yaml-mode settings
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; json-mode settings
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'json-mode-hook
          '(lambda ()
             (setq show-trailing-whitespace t)
             (setq js-indent-level 2)
             (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(setq pylint-names (list "pylintrc" "pylintrc.conf" ".pylintrc"))
(setq pylintrc-cache nil)

(defun find-pylintrc (file-name)
  """ find the pylintrc for a project """
  (setq buffer-directory (file-name-directory file-name))
  (if (not (assoc buffer-directory pylintrc-cache))
      (progn
        (setq gitroot
              (chomp (shell-command-to-string
                      (concat "cd " buffer-directory
                              " && git rev-parse --show-toplevel"))))
        (setq pylintrc
              (chomp (shell-command-to-string
                      (concat "find " gitroot " -type f \\( -name "
                              (mapconcat 'identity pylint-names " -o -name ")
                              " \\) -print -quit"))))
        (add-to-list 'pylintrc-cache
                     (cons buffer-directory
                           (if (string= "" pylintrc) nil pylintrc)))))
  (cdr (assoc buffer-directory pylintrc-cache)))

;; flymake settings
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/bin/pychecker.py" (list local-file))))
  (defun flymake-rst-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "rstchecker" (list local-file))))
  (defun flymake-sh-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "shchecker" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.rst\\'" flymake-rst-init))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.sh\\'" flymake-sh-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)


;; better flymake faces
(custom-set-faces
  '(flymake-errline ((((class color)) (:background "IndianRed1"))))
  '(flymake-warnline ((((class color)) (:background "gold1")))))

;; keep state for flymake-display-err-in-minibuffer so multiple
;; invocations cycle through the errors for a given line
(setq flymake-error-idx 0)
(setq flymake-error-line 0)

(defun flymake-display-err-in-minibuffer ()
  (interactive)
  (let* ((line-no (flymake-current-line-no))
         (line-err-info-list (car (flymake-find-err-info flymake-err-info
                                                         line-no))))
    (if line-err-info-list
        (progn
          (if (= line-no flymake-error-line)
              (setq flymake-error-idx (1+ flymake-error-idx))
            (setq flymake-error-idx 0))
          (setq flymake-error-line line-no)
          (setq err (nth flymake-error-idx line-err-info-list))
          (if err nil
            (progn
              ;; went past the end of the error list -- start over
              (setq flymake-error-idx -1)
              (setq err (car line-err-info-list))))
          (princ (format "%s (%d)" (flymake-ler-text err) line-no)))
      (princ (format "No errors on line %d" line-no)))))

(defun flymake-pylint-error-list (err-info-list)
  (if err-info-list
      (let ((err-text (flymake-ler-text (car err-info-list))))
        (if (string-match "^\\[\\([^,]*\\)," err-text)
            (cons (match-string 1 err-text)
                  (flymake-pylint-error-list (cdr err-info-list)))
          (flymake-pylint-error-list (cdr err-info-list))))
    '()))

(defun flymake-disable-pylint-on-line ()
  (interactive)
  (end-of-line)
  (insert
   (format "  # pylint: disable=%s"
           (mapconcat
            'identity
            (flymake-pylint-error-list
             (car
              (flymake-find-err-info flymake-err-info
                                     (flymake-current-line-no))))
            ","))))

(define-prefix-command 'flymake-map)
(global-set-key "\C-xe" 'flymake-map)
(define-key flymake-map "s" 'flymake-display-err-in-minibuffer)
(define-key flymake-map "d" 'flymake-disable-pylint-on-line)
(define-key flymake-map "p" 'flymake-goto-prev-error)
(define-key flymake-map "n" 'flymake-goto-next-error)

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
