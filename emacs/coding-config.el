;;; coding-config.el ---

;; Text Mode
(setq text-mode-hook '(lambda nil (setq fill-column 80) (auto-fill-mode 1)))

;; Compilation Mode
(setq compilation-scroll-output 1)
(add-hook 'compilation-mode-hook '(lambda () (setq truncate-lines nil)))

;; Shell Mode
(setq comint-prompt-read-only t)
(add-hook 'shell-mode-hook '(lambda ()
                              (toggle-truncate-lines 1)
                              (setq show-trailing-whitespace nil)))
(setq ansi-color-names-vector
      ["black" "red4" "green4" "yellow4" "DarkSlateGray2" "magenta4" "cyan4" "white"])

(condition-case ()
    (progn
      (require 'ansi-color)
      (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
      (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
      (setq ansi-color-map (ansi-color-make-color-map)))
  (error (message "Skipping ansi-color")))

(add-hook 'shell-mode-hook
          '(lambda ()
             (setq dirtrack-list '("^\\[[a-zA-Z0-9@]+ \\(.*\\) (.*)\\]" 1 nil))
             (dirtrack-mode 1)))

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

(add-hook 'python-mode-hook
	  (lambda()
            (abbrev-mode 1)
	    (define-key python-mode-map "\C-m" 'newline-and-indent)
            (define-key python-mode-map "\C-c\C-b" 'python-send-buffer)
            (define-key python-mode-map "\C-c\C-e" 'python-send-defun)
	    (setq python-indent 4)
            (flyspell-prog-mode)
            (turn-on-eldoc-mode)
            (abbrev-mode -1)
            (setq show-trailing-whitespace t)))

(provide 'coding-config)
