;;; coding-config.el ---

;; Text Mode
(setq text-mode-hook '(lambda nil (setq fill-column 80) (auto-fill-mode 1)))

;; Compilation Mode
(setq compilation-scroll-output 1)
(require 'ansi-color)
(add-hook 'compilation-mode-hook '(lambda () (setq truncate-lines nil)))
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;; Coding modes
(dolist (mode '(c++-mode java-mode python-mode thrift-mode))
  (font-lock-add-keywords
   mode
   `((,(format "^%s\\(.+\\)" (make-string 80 ?.))
      (1 font-lock-warning-face t)))
   t ) )

(add-hook 'c-mode-common-hook
	  (lambda()
	    (define-key c++-mode-map "\C-m" 'newline-and-indent)
            (flyspell-prog-mode)
            (setq show-trailing-whitespace t)))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'emacs-lisp-mode-hook
	  (lambda()
            (require 'eldoc)
            (turn-on-eldoc-mode)
            (define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)
            (define-key emacs-lisp-mode-map "\C-x:r" 'eval-region)
            (define-key emacs-lisp-mode-map "\C-x:b" 'eval-buffer)
            (define-key emacs-lisp-mode-map "\C-x:d" 'eval-defun)
            (setq show-trailing-whitespace t)))

(add-hook 'lisp-interaction-mode-hook
          (lambda()
            (local-set-key (kbd "M-RET") 'eval-print-last-sexp)))

(add-hook 'python-mode-hook
	  (lambda()
	    (define-key python-mode-map "\C-m" 'newline-and-indent)
            (define-key python-mode-map "\C-c\C-b" 'python-send-buffer)
            (define-key python-mode-map "\C-c\C-e" 'python-send-defun)
	    (setq python-indent 4)
            (flyspell-prog-mode)
            (turn-on-eldoc-mode)
            (abbrev-mode -1)
            (setq show-trailing-whitespace t)))

(provide 'coding-config)
