;;; bootstrap.el ---

(defconst local-package-dir "~/.dotfiles/emacs")

;; Install packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

(setq url-proxy-services
       '(("no_proxy" . "^\\(localhost\\|10.*\\)")
         ("http" . "fwdproxy.any:8080")
         ("https" . "fwdproxy.any:8080")))

(defun ensure-package-installed (&rest packages)
    "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
    (mapcar
     (lambda (package)
       ;; (package-installed-p 'evil)
       (if (package-installed-p package)
           nil
         (if (y-or-n-p (format "Package %s is missing. Install it? " package))
             (package-install package)
           package)))
     packages))

;; make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'use-package)

(use-package ivy
  :bind (("C-c v" . ivy-resume)
         ("C-c C-v" . ivy-resume)
         ("C-b" . ivy-switch-buffer)
         ("C-v" . ivy-switch-buffer)
         ("M-e" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-v". ivy-restrict-to-matches)
         ("C-s". ivy-next-line)
         ("C-r". ivy-previous-line)
         ("C-c o". ivy-occur)
         ("C-c C-o". ivy-occur)
         ("M-y". yank-pop)
         ("RET". ivy-alt-done)
         ("M-RET". ivy-done)
         ("M-!". ivy-dispatching-done)
         ("<f2>" . hydra-ivy/body))
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
	;; allow input not in order
        '((t   . ivy--regex-ignore-order))))

(use-package counsel
  :bind
  (("M-y" . counsel-yank-pop)
   ("M-x"  . counsel-M-x)
   ("C-x C-m"  . counsel-M-x)
   ("C-c C-m" . counsel-M-x)
   ("C-x m" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> c" . counsel-describe-face)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line))

  :ensure t
  :config
  (setq ivy-extra-directories nil)
  (counsel-mode 1))

(use-package swiper
  :bind
  (("M-s" . swiper)
   :map swiper-map
   ("C-r" . swiper-query-replace)
   ("M-%" . swiper-query-replace)
   )
  :config
  (require 'subr-x)
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (defconst color-theme-sanityinc-tomorrow-colors
    '((vibrant . ((background . "#2d2d2d")
                  (current-line . "#393939")
                  (selection . "#3e4446")
                  (foreground . "#cccccc")
                  (comment . "#999999")
                  (red . "#f2777a")           ; warning
                  (orange . "#60DCFF")        ; function name
                  (yellow . "#66cccc")        ; variable name
                  (green . "#ffcc66")         ; keyword
                  (aqua . "#99cc99")          ; string
                  (purple . "#cc99cc")        ; builtin
                  (blue . "color-39")))))       ; type

  (defun color-theme-sanityinc-tomorrow-vibrant ()
    "Apply the tomorrow blue theme."
    (interactive)
    (color-theme-sanityinc-tomorrow 'vibrant))
  (color-theme-sanityinc-tomorrow--define-theme vibrant)
  (color-theme-sanityinc-tomorrow-vibrant)
  ;; overwrite more face mapping.
  (set-face-foreground 'font-lock-constant-face "color-158")
  (set-face-foreground 'font-lock-type-face "#cc99cc")
  (set-face-foreground 'font-lock-preprocessor-face "orange")
  (setq custom-enabled-themes '(sanityinc-tomorrow-vibrant))
  (setq custom-safe-themes
    '("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq spaceline-process-p nil)
  (setq spaceline-minor-modes-p nil)
  (setq spaceline-buffer-encoding-abbrev-p nil)
  (setq spaceline-version-control-p nil)
  (setq spaceline-window-numbers-unicode nil)
  (custom-set-faces
   '(mode-line-buffer-id ((t (:weight bold :foreground "color-240"))))
   '(powerline-active1 ((t (:inherit mode-line :background "grey22" :foreground "color-39"))))
   '(powerline-inactive1 ((t (:inherit mode-line :background "black" :foreground "#a8a8a8"))))
   '(powerline-inactive2 ((t (:inherit mode-line :background "grey22" :foreground "#a8a8a8"))))
   )
  (spaceline--theme
   '((window-number buffer-modified) :face highlight-face :separator " | ")
   '(buffer-id :face highlight-face)))

(use-package smooth-scrolling
  :ensure t)

(use-package markdown-mode
  :mode "\\.md\\'"
  :init
  (defun my-markdown-mode-hook ()
    (setq-local truncate-lines nil)
    (setq-local set-fill-column 80))
  (add-hook 'markdown-mode-hook 'my-markdown-mode-hook)
  :ensure t
  :defer t)

(use-package flx
  :ensure t
  :defer t)

(use-package wgrep
  :ensure t
  :defer t)

(use-package buffer-stack
  :bind
  (("M-<left>" . buffer-stack-down)
   ("M-<right>" . buffer-stack-up))
  :ensure t)

(use-package ace-window
  :bind
  (("M-<up>" . ace-window))
  :ensure t)

(use-package window-numbering
  :init
  (defvar window-numbering-keymap (make-sparse-keymap))
  :bind
  (("C-c 1" . select-window-1)
   ("C-c 2" . select-window-2)
   ("C-c 3" . select-window-3)
   ("C-c 4" . select-window-4)
   ("C-c 5" . select-window-5))
  :ensure t
  :config
  (window-numbering-mode)
  (window-numbering-clear-mode-line))

(use-package windmove
  :bind
  (("C-S-<left>" . windmove-left)
   ("C-S-<right>" . windmove-right)
   ("C-S-<down>" . windmove-down)
   ("C-S-<up>" . windmove-up))
  :ensure t)

(use-package recentf
  :config
  (recentf-mode 1)
  (run-at-time nil (* 12 3600) 'recentf-save-list)
  :init
  (setq recentf-max-menu-items 10)
  (setq recentf-max-saved-items 1000)
  (setq recentf-save-file "~/.emacs.d/recentf"))

(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (setq company-dabbrev-code-other-buffers (quote all))
  (setq company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                  (company-dabbrev-code company-gtags company-keywords)
                  company-oddmuse company-dabbrev))))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yatemplate
  :ensure t)

(use-package shell
  :demand
  :bind
  (:map shell-mode-map
   ("M-r" . counsel-shell-history))
  :init
  (setenv "ESHELL" "~/bin/zsh")
  (setenv "PAGER" "cat")
  (add-hook 'shell-mode-hook 'dirtrack-mode)
  (setq comint-prompt-read-only t)
  (setq dirtrack-list '("^%[^a-z0-9.]+\\[[a-z]+ \\(.*\\)\\]" 1))
  (add-hook 'shell-mode-hook (lambda()
                               (setq-local yas-fallback-behavior '(apply company-manual-begin . ()))))

  (defun new-shell (shellName)
    "Creates a new shell "
    (interactive "sEnter Shell Name:")
    (save-excursion
      (shell)
      (comint-send-input)
      (rename-buffer shellName)))

  (defun switch-to-shell ()
    "Switches to the shell specified by the last character typed"
    (interactive)
    (let* ((keys (recent-keys))
           (len (length keys))
           (key1 (if (> len 0)
                     (string (event-basic-type (elt keys (- len 1))))
                   0))
           (shell-buffer-name (concat "shell-" key1)))
      (if (get-buffer shell-buffer-name)
          (switch-to-buffer shell-buffer-name)
        (new-shell shell-buffer-name))))
  (require 'cl)
  (loop for i from 1 to 5 do
        (global-set-key (read-kbd-macro (format "M-%d" i)) 'switch-to-shell))
  :config
  (require 'ansi-color)
  (setq ansi-color-names-vector
        ["black" "red4" "green4" "yellow4" "DarkSlateGray2" "magenta4" "cyan4" "white"])
  (ansi-color-for-comint-mode-on))

(use-package rtags
  :config
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  ;;(setq rtags-completions-enabled t)
  ;;(push 'company-rtags company-backends)
  (rtags-enable-standard-keybindings c-mode-base-map "M-r")
  :ensure t)

(use-package zzz-to-char
  :bind (("M-z" . zzz-to-char))
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-c e" . mc/edit-lines)))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-right-bottom))


(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list
  '("☯"
    "✪⚪"
    "✤"
    "✿"
    )))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-log-done t)
  (setq org-hide-leading-stars t)
  :init
  (add-hook 'org-mode-hook (lambda()
                             (org-bullets-mode 1)
                             (org-defkey org-mode-map (kbd "M-S-return")  'org-insert-todo-heading-respect-content)
                             (org-defkey org-mode-map (kbd "<C-up>")      'org-metaup)
                             (org-defkey org-mode-map (kbd "<C-down>")    'org-metadown)
                             (org-defkey org-mode-map (kbd "<C-left>")    'org-metaleft)
                             (org-defkey org-mode-map (kbd "<C-right>")   'org-metaright)
                             (org-defkey org-mode-map (kbd "M-<right>")   'nil)
                             (org-defkey org-mode-map (kbd "M-<left>")    'nil))))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :bind
  (:map python-mode-map
        ("M-S-<RET>" . electric-newline-and-maybe-indent)
        ("M-`" . python-shell-switch-to-shell))
  :init
  (add-hook 'python-mode-hook (lambda()
                                (flyspell-prog-mode)
                                (turn-on-eldoc-mode)
                                (setq show-trailing-whitespace t)
                                )))

(use-package ui-config
  :load-path local-package-dir
  :demand)

(use-package key-config
  :load-path local-package-dir

  :demand

  :init
  (setq sticky-map (make-sparse-keymap))
  (setq emulation-mode-map-alists (list (list (cons 'sticky-map sticky-map))))
  ;; meta-shift-return is not defined by the normal xterm input-decode-map.
  ;; this is also our own key code. not standard compliant.
  (define-key input-decode-map "\e[13;8u" [M-S-return])
  (define-key input-decode-map "\e\e[13;8u" [M-S-return])

  :bind
  (("C-s" . isearch-forward-regexp)
   ("C-r" . isearch-backward-regexp)
   ("C-q" . kill-syntax-backward)
   ("C-d" . delete-char)
   ("M-d" . kill-syntax-forward)
   ("M-g" . goto-line)
   ("M-o" . find-file-at-point)
   ("C-c l" . sort-lines)
   ("C-x C-g" . keyboard-quit)
   ("<f11>" . revert-no-confirm)
   ("<f12>" . cycle-buffer)
   ("<f6>" . next-error)
   ("<f9>" . repeat-complex-command)
   ("M-i" . indent-region)
   ("M-%" . query-replace-regexp)
   ("<f3>" . query-replace-regexp)
   ("M-DEL" . sc-join-line)
   ("M-`" . cycle-window)

   :map sticky-map
   ("C-j". next-line)
   ("C-k". previous-line)
   ("C-l". forward-char)
   ("C-h". backward-char)
   ("M-l". forward-word)
   ("M-h". backward-word)
   ("M-j". scroll-up)
   ("M-k". scroll-down)
   ("C-o". other-window)
   ("M-d". kill-line)
   ("M-8" . match-paren)
   ("M-e" . ivy-switch-buffer)

   :map c-mode-base-map
   ("C-c c" . recompile)
   ("C-c C-c" . recompile)
   ("C-c C-r" . rotate-among-files)
   ("C-c r" . rotate-among-files)))

(use-package company-flow
  :ensure t
  :config
  (add-to-list 'company-backends 'company-flow))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)

  (defun recenter-to-reposition (orig-fun &rest args)
    (let ((recenter reposition-window))
      (apply orig-fun args)))

  ;; flycheck has this bad flickering affect in console mode due to a
  ;; lot of recentering of the current error in the error list window.
  (advice-add 'flycheck-error-list-recenter-at :around #'recenter-to-reposition))

(use-package flycheck-flow
  :ensure t)

(require 'flycheck-rtags)

(use-package web-mode
  :mode ("\\.js\\'" . web-mode)
  :mode ("\\.css\\'" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-content-types-alist
      '(("jsx"    . "\\.react.js\\'")))
  (flycheck-add-mode 'javascript-flow 'web-mode)
  :bind
  (("<f12>" . flycheck-list-errors))
  :ensure t)

(use-package coding-config
  :demand
  :load-path local-package-dir)

(use-package local-config
  :demand
  :load-path local-package-dir)


(server-start nil)

(provide 'bootstrap)
