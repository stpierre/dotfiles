(princ "Loading ~/.emacs")

;; don't display the annoying splash screen
(setq inhibit-splash-screen t)

;; open Alpine temp files (pico.NNNNN) in text-mode
(setq auto-mode-alist (append '(("pico\\." . text-mode))
                              auto-mode-alist))

;; enable text-fill by default (good for composing email)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'tbemail-mode-hook 'turn-on-auto-fill)

;; set M-x compile to something handier.  this lets you run C-x C-m
;; C-m to compile
(global-set-key "\C-x\C-m" 'compile)

;; ensure that M-g, g is set as goto-line
(global-set-key "\M-gg" 'goto-line)

;; set C-c, c to comment-region and C-c, u to uncomment-region
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)

;; set C-c, s to point-to-register ([s]ave the current point to the
;; register) and C-c, j to [j]ump-to-register
(global-set-key "\C-cs" 'point-to-register)
(global-set-key "\C-cj" 'jump-to-register)

;; set user-emacs-directory on older versions of emacs
(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory "~/.emacs.d"))
(if (file-exists-p user-emacs-directory)
    (add-to-list 'load-path user-emacs-directory))

;; perl mode settings
(add-hook 'perl-mode-hook
          '(lambda () 
             (local-set-key "\C-c\C-k" 'perlcritic)
             (require 'compile)
             (hs-minor-mode)
             (set (make-local-variable 'compile-command)
                  (concat "perl -c "
                          (file-name-nondirectory buffer-file-name)))))

;; xml mode settings
(setq auto-mode-alist (append '(("\\.xsd\\'" . xml-mode))
                              auto-mode-alist))
(add-hook 'sgml-mode-hook
          '(lambda () 
             (setq tab-width 4)
             (turn-on-auto-fill)))

;; org mode settings
(add-hook 'org-mode-hook
          '(lambda () 
             (org-indent-mode)
             (local-set-key "\C-cl" 'org-store-link)))

(eval-after-load "perlcritic"
  '(perlcritic-severity 4))

;; python mode settings
(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key "\C-c\C-k" 'pylint)
             (setq tab-width 4)
             (hs-minor-mode)
             (setq python-indent 4)
             (unless (assoc 'python-mode hs-special-modes-alist)
               (setq hs-special-modes-alist
                     (cons
                      (list 'python-mode
                            "^\\s-*def\\>" nil "#" 
                            (lambda (arg)
                              (py-end-of-def-or-class)
                              (skip-chars-backward " \t\n"))
                            nil)
                      hs-special-modes-alist)))))

;; PHP mode settings
(add-hook 'php-mode-hook
          '(lambda () 
             (setq tab-width 4)
             (require 'compile)
             (hs-minor-mode)
             (set (make-local-variable 'compile-command)
                  (concat "php -l "
                          (file-name-nondirectory buffer-file-name)))))

;; Genshi template settings
(if (file-exists-p (concat user-emacs-directory "genshi-mode.el"))
    (progn
      (autoload 'genshi-mode
        (concat user-emacs-directory "genshi-mode.el")
        "Genshi mode." t)
      (setq auto-mode-alist (append '(("\\.newtxt\\'" . genshi-mode))
                                    auto-mode-alist))
      (setq auto-mode-alist (append '(("\\.genshi\\'" . genshi-mode))
                                    auto-mode-alist))))

;; CSS mode settings
(add-hook 'css-mode-hook
          '(lambda ()
             (setq cssm-indent-level 4)
             (setq cssm-newline-before-closing-bracket t)
             (setq cssm-indent-function #'cssm-c-style-indenter)
             (setq cssm-mirror-mode nil)))

;; tabs suck, don't use them
(setq-default indent-tabs-mode nil)

;; search case-insensitive by default
(setq-default case-fold-search t)

;; enable the upcase-region command without confirmation
(put 'upcase-region 'disabled nil)

;; load RPM specfile mode
(if (file-exists-p (concat user-emacs-directory "rpm-spec-mode.el"))
    (progn
      (autoload 'rpm-spec-mode
        (concat user-emacs-directory "rpm-spec-mode.el")
        "RPM spec mode." t)
      (setq auto-mode-alist
            (append '(("\\.spec" . rpm-spec-mode)) auto-mode-alist))))

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
(setq user-mail-address "chris.a.st.pierre@gmail.com")
(setq user-full-name "Chris St. Pierre")

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
                    '(height . 80))
              default-frame-alist))
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

;; load graphviz mode
(if (file-exists-p (concat user-emacs-directory "graphviz-dot-mode.el"))
    (progn
      (autoload 'graphviz-dot-mode
        (concat user-emacs-directory "graphviz-dot-mode.el")
        "Graphviz mode." t)
      (add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
      (add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))))

;; graphviz mode settings
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
  (kill-emacs)
  )

;; create a python-scratch buffer that's just like *scratch*, but with
;; the python major mode
(with-current-buffer
    (generate-new-buffer "*python-scratch*")
  (python-mode))

;; Thunderbird External Editor mode
(require 'tbemail)
