;;; tui.el --- Terminal (TTY) frames -*- lexical-binding: t -*-

;;; Commentary:
;;; Mouse, clipboard, and full keyboard support in terminal frames,
;;; tuned for kitty.  Cmd is Meta and Option is Super, as in GUI frames
;;; (see mac.el); that needs kkp here plus the matching kitty.conf
;;; settings, which pass Cmd keys through while Emacs has focus.

;;; Code:

(setq xterm-set-window-title t
      ;; copy to the system clipboard with OSC 52.  Leave out
      ;; modifyOtherKeys, since kkp replaces it.
      xterm-extra-capabilities '(setSelection))

(xterm-mouse-mode 1)

;; kitty keyboard protocol
(use-package kkp
  :ensure t
  :custom
  (kkp-super-modifier 'meta)
  (kkp-alt-modifier 'super)
  :hook (tty-setup . global-kkp-mode))

(defun kitty-set-in-emacs (terminal in-emacs)
  "Set the in_emacs user variable on kitty TERMINAL.
Set it if IN-EMACS is non-nil, and unset it otherwise.  kitty.conf
uses the variable to pass Cmd keys through to Emacs."
  (when (and (eq (terminal-live-p terminal) t)
             (equal (tty-type terminal) "xterm-kitty"))
    (send-string-to-terminal
     (if in-emacs
         "\e]1337;SetUserVar=in_emacs=MQ==\a"
       "\e]1337;SetUserVar=in_emacs\a")
     terminal)))

(defun kitty-enter-emacs (&optional terminal)
  "Tell kitty that Emacs has TERMINAL (default: the selected one)."
  (kitty-set-in-emacs (or terminal (frame-terminal)) t))

(defun kitty-leave-emacs (terminal)
  "Tell kitty that Emacs no longer has TERMINAL."
  (kitty-set-in-emacs terminal nil))

(defun kitty-leave-emacs-everywhere ()
  "Tell kitty that Emacs no longer has any of its terminals."
  (mapc #'kitty-leave-emacs (terminal-list)))

(add-hook 'tty-setup-hook #'kitty-enter-emacs)
(add-hook 'resume-tty-functions #'kitty-enter-emacs)
(add-hook 'suspend-tty-functions #'kitty-leave-emacs)
(add-hook 'delete-terminal-functions #'kitty-leave-emacs)
(add-hook 'kill-emacs-hook #'kitty-leave-emacs-everywhere)

;;; tui.el ends here
