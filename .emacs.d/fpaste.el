;;; fpaste.el --- Emacs support for fpaste.org

;; Copyright (C) 2011  Raul Gutierrez Segales
;; URL: http://people.collabora.co.uk/~rgs/elisp/fpaste.el
;; Licensed under the GPL.
;; Modified 2012 Chris St. Pierre

;; todo: make this pass -l <language> to fpaste based on the buffer
;; mode

(defun shell-command-on-region-to-string (start end command)
  ;; shell-command-on-region sends output to a buffer by default; send
  ;; it to a string instead
  (save-window-excursion
    (with-output-to-string
      (shell-command-on-region start end command standard-output))))

(defun do-paste ()
  (progn
    (setq fpaste-command
          (concat "fpaste"
                  (if (boundp 'user-full-name)
                      (concat " -n '" user-full-name "'"))
                  " -d '" (buffer-name) "'"
                  "2>&1"))
    (setq fpaste-output
          (shell-command-on-region-to-string
           (region-beginning)
           (region-end)
           fpaste-command))
    (dolist (line (split-string fpaste-output "\n"))
      (when (string-match "http" line)
        (progn
          (message line)
          (kill-new line))))
    ))

(defun fpaste-region ()
  "fpaste a region"
  (interactive)
  (let (ack)
    (setq ack (read-from-minibuffer "Paste [y|n]?: "))
    (if (equal ack "y")
        (do-paste))
    ))

(global-set-key "\C-c\C-f" 'fpaste-region)

(provide 'fpaste)
