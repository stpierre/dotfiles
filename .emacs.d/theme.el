;;; theme.el --- Theme, frames, and the mode line -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package solarized-theme
  :ensure t
  :demand t
  :config (load-theme 'solarized-dark t))

(put 'scroll-left 'disabled nil)

;; set initial window size
(add-to-list 'default-frame-alist '(width . 164))
(add-to-list 'default-frame-alist '(height . 80))

(setq inhibit-startup-message t)

(size-indication-mode 1)
(column-number-mode 1)

(setq-default mode-line-position
              '((-3 "%p") (size-indication-mode ("/" (-4 "%I")))
                " "
                (line-number-mode
                 ("%l" (column-number-mode ":%c")))))

;; show the branch and status in the mode line, without the "Git" prefix
(setq vc-display-status 'no-backend)
(setq-default mode-line-format
              (list ""
                    'mode-line-modified " "
                    'mode-line-buffer-identification
                    '(vc-mode vc-mode) " "
                    'mode-line-position " "
                    'mode-line-modes
                    'mode-line-misc-info))

;; the toolbar and menu bar are wastes of valuable screen estate
(tool-bar-mode -1)
(menu-bar-mode -1)

;; the blinking cursor is nothing but an annoyance
(blink-cursor-mode -1)

;; show colors like #268bd2 in the color they describe
(use-package colorful-mode
  :ensure t
  :hook (after-init . global-colorful-mode))

;; indentation guides; characters in TTY frames, stipple bars in GUI ones
(use-package indent-bars
  :ensure t
  :custom
  ;; set up after mode hooks, file/dir-locals and editorconfig have
  ;; settled the indentation (e.g. 2-column tabs in go.el)
  (indent-bars-defer-setup t)
  (indent-bars-treesit-support t)
  ;; no bars on blank lines between top-level forms
  (indent-bars-treesit-ignore-blank-lines-types '("module" "source_file"))
  ;; de-emphasize bars outside the innermost block around point
  (indent-bars-treesit-scope
   '((python function_definition class_definition for_statement
             if_statement with_statement while_statement try_statement)
     (go function_declaration method_declaration func_literal
         for_statement if_statement expression_switch_statement
         type_switch_statement select_statement)))
  :hook ((prog-mode yaml-ts-mode) . indent-bars-mode))

;;; theme.el ends here
