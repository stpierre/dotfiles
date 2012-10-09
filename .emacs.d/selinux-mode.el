;; SELinux TE File mode
;; Author: Ashish Shukla <gmail.com!wahjava>
;; Copyright 2008. Ashish Shukla
;; licensed under the same license as Emacs i.e. GPLv2

(defvar selinux-te-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.te\\'" . selinux-te-mode))

(defconst selinux-te-font-lock-keywords-1
  (list
   '("\\<\\(?:a\\(?:l\\(?:ias\\|low\\)\\|ttribute\\|udit\\(?:allow\\|deny\\)\\)\\|bool\\|c\\(?:ategory\\|l\\(?:ass\\|one\\)\\|o\\(?:mmon\\|nstrain\\)\\)\\|do\\(?:m\\(?:by\\|inance\\|\\)\\|ntaudit\\)\\|else\\|fs\\(?:con\\|_use_\\(?:xattr\\|task\\|trans\\)\\)\\|genfscon\\|i\\(?:f\\|n\\(?:herits\\|comp\\)\\)\\|level\\|m\\(?:odule\\|ls\\(?:constrain\\|validatetrans\\)\\)\\|n\\(?:e\\(?:tifcon\\|verallow\\)\\|odecon\\)\\|optional\\|p\\(?:ath\\|ortcon\\)\\|r\\(?:ange\\(?:_transition\\|\\)\\|equire\\|ole\\(?:s\\|_transition\\|\\)\\)\\|s\\(?:e\\(?:lf\\|nsitivity\\)\\|id\\|ource\\)\\|t\\(?:arget\\|ype\\(?:a\\(?:lias\\|ttribute\\)\\|s\\|_\\(?:transition\\|member\\|change\\)\\|\\)\\)\\|u\\(?:ser\\|1\\|2\\|3\\)\\|r\\(?:1\\|2\\|3\\)\\|t\\(?:1\\|2\\|3\\)\\|l\\(?:1\\|2\\)\\|h\\(?:1\\|2\\)\\|validatetrans\\)\\>" . font-lock-keyword-face)
   '(":\\<\\(?:f\\(?:d\\|ile\\(?:system\\|\\)\\)\\|\\(?:\\(?:blk\\|chr\\|fifo\\|lnk\\|sock\\)_\\)file\\|d\\(?:ir\\|bus\\|rawable\\)\\|\\(?:\\(key\\|\\(?:netlink\\(?:_route\\|_firewall\\|_tcpdiag\\|_nflog\\|_xfrm\\|_selinux\\|_audit\\|_ip6fw\\|_dnrt\\|_kobject_uevent\\)?\\)\\|packet\\|\\(?:rawi\\|tc\\|ud\\)p\\|unix_\\(?:dgram\\|stream\\)\\|appletalk\\)_\\)?socket\\|ipc\\|msgq\\|s\\(?:e\\|h\\)m\\|capability\\|msg\\|n\\(?:etif\\|ode\\|scd\\)\\|p\\(?:ro\\(cess\\|perty\\)\\|a\\(?:x\\|sswd\\)\\)\\|s\\(?:ecurity\\|ystem\\)\\|x\\(?:client\\|input\\|server\\|extension\\)\\|font\\|c\\(?:olormap\\|ursor\\)\\)\\>" . font-lock-builtin-face)

   '("\\(\\w*\\)" . font-lock-variable-name-face))
  "Default font-lock-keywords for `selinux-te-mode'.")

(defconst selinux-m4-te-font-lock-keywords-1
  (append '(
	    ("\\<\\(?:gen_\\(?:\\(?:requir\\|tunabl\\)e\\)\\|optional_policy\\|policy_module\\|tunable_policy\\)\\>" . font-lock-keyword-face)
;("gen_\\(?:\\(?:requir\\|tunabl\\)e\\)\\|\\(?:optional\\|tunable\\)_policy\\|policy_module" . font-lock-keyword-face)
	    ) selinux-te-font-lock-keywords-1))

;; modify this syntax table list
(setq selinux-te-mode-syntax-table-list
      '((?{ "(}")
	(?} "){")
	(?# "<\n")
	(?\n ">#")
	(?_ "w")
	(?- "w")
	(?\; ".")))

(defvar selinux-te-mode-syntax-table
  (let ((st (make-syntax-table)))
    (dolist (syntax-table-entry selinux-te-mode-syntax-table-list st)
      (modify-syntax-entry (car syntax-table-entry) (car (cdr syntax-table-entry)) st)))
  "Syntax table used in `selinux-te-mode'.")

(defun selinux-te-indent-line()
  "Indents SELinux TE Line"
  (interactive)
  (beginning-of-line)
  (let (cur-indent (indented nil))
      
    ;; if at beginning of buffer, then indent to column 0
    (if (bobp)
	(progn (setq cur-indent 0) (setq indented t))

      ;; else
      (progn
	(save-excursion

	  ;; now indentation depends on what previous line, so move back
	  (forward-line -1)
	    
	  ;; if its something like, (require|common|class|option) word {, then
	  (if (looking-at "^[ \t]*\\(require\\|common\\|optional\\|class\\)[ \t]*[\\w_]*{[ \t]*$")

	      ;; indent-by current tab-width
	      (progn
		(setq cur-indent (+ (current-indentation) tab-width))
		(setq indented t))
	      
	    ;; else if its some other style, like in "allow { httpd_t ftpd_t", then
	    (if (looking-at "^.*{[^}]*$")
		(progn
		  ;; indent to the column no. of '{' + 1
		  (setq cur-indent (progn (search-forward "{") (+ (current-column) 1)))
		  (setq indented t))

	      ;; else use current-indentation
	      (progn
		(setq cur-indent (current-indentation))
		(setq indented   t)
		)
	      )
	    )
	  )
	  
	;; now, we're back at current line, see if current line has '}', then we might need to de-indent.
	(if (looking-at "^[ \t]*}")
	    (save-excursion
	      ;; move to the point where it all started, i.e. the line containing '{'
	      ;; so start moving upwards, unless we find that line
	      ;; this block of code also keeps track of matching of braces
	      (let ((brace-nesting-count 0) (reached-start nil))

		(while (or (> brace-nesting-count 0) (not reached-start))
		  (if (looking-at "^.*}") 
		      (setq brace-nesting-count (+ brace-nesting-count 1))
		    (if (looking-at "^.*{")
			(setq brace-nesting-count (- brace-nesting-count 1))
		      )
		    )

		  (forward-line -1)
		  (setq reached-start (bobp))
		  (if (= brace-nesting-count 0) (progn (forward-line 1)
						       (setq reached-start t)))
		  )
		)
		  
	      ;; now we're at that line, so see, if its some 'require|common|optional|class' block,
	      ;; if it is, then
	      (if (looking-at "^[ \t]*\\(require\\|common\\|optional\\|class\\)[ \t]*[\\w_]*{[ \t]*$")

		  ;;  set the indentation to the current-indentation.
		  (setq cur-indent (current-indentation))

		;; else, if its some other style, then indent to the column no. of '{'
		(progn
		  (setq cur-indent (progn (search-forward "{") (- (current-column) 1)))
		  (setq indented t)
		  )
		)
	      )
	  )
	)
      )
      
    (if indented (indent-line-to cur-indent))
    )
  )
  
(defun selinux-te-mode()
  "Major mode for selinux .te files"
  (interactive)
  (kill-all-local-variables)
   
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'comment-start)
  (make-local-variable 'indent-line-function)

  (setq major-mode 'selinux-te-mode
	mode-name "SELinux TE"
	font-lock-defaults '(selinux-te-font-lock-keywords-1 nil)
	comment-start "#"
	indent-line-function 'selinux-te-indent-line
	)

  (set-syntax-table selinux-te-mode-syntax-table)
  (run-mode-hooks 'selinux-te-mode-hook)
  )

(require 'm4-mode)
(define-derived-mode selinux-m4-te-mode
  m4-mode
  "SELinux TE (m4)"
  (set (make-local-variable 'm4-font-lock-keywords)
       (append selinux-m4-te-font-lock-keywords-1 m4-font-lock-keywords))
  (set
   (make-local-variable 'm4-mode-syntax-table)
   (let ((st nil))
     (dolist (syntax-table-entry selinux-te-mode-syntax-table-list st)
       (modify-syntax-entry (car syntax-table-entry)
			    (car (cdr syntax-table-entry))
			   st)))))

(provide 'selinux-te-mode)
(provide 'selinux-m4-te-mode)
