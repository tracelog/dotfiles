;;; bootstrap.el ---

;; Install packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

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
         ("M-t" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-v". ivy-restrict-to-matches)
         ("C-s". ivy-next-line)
         ("C-r". ivy-previous-line))
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
   ("M-t" . counsel-recentf)
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
  (counsel-mode 1))

(use-package swiper
  :bind
  (("M-s" . swiper)
   :map swiper-map
   ("C-r" . swiper-query-replace)
   )
  :config
  (require 'subr-x)
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
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
  (set-face-foreground 'font-lock-preprocessor-face "orange"))

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
  :config
  (defun my-markdown-mode-hook ()
    (setq truncate-lines nil)
    (setq set-fill-column 80))
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
  (("ESC <left>" . buffer-stack-down)
   ("ESC <right>" . buffer-stack-up))
  :ensure t)

(use-package ace-window
  :bind
  (("C-M-O" . ace-window))
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
  (("C-M-h" . windmove-left)
   ("C-M-l" . windmove-right)
   ("C-M-j" . windmove-down)
   ("C-M-k" . windmove-up))
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
  (global-company-mode 1))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yatemplate
  :ensure t)

(use-package shell
  :bind
  (:map shell-mode-map
   ("M-r" . counsel-shell-history))
  :init
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
  (setq ansi-color-names-vector
        ["black" "red4" "green4" "yellow4" "DarkSlateGray2" "magenta4" "cyan4" "white"]))

(use-package rtags
  :config
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (push 'company-rtags company-backends)
  (rtags-enable-standard-keybindings c-mode-base-map "M-r")
  :ensure t)

(use-package zzz-to-char
  :bind (("M-z" . zzz-to-char))
  :ensure t)

(server-start nil)

(provide 'bootstrap)
