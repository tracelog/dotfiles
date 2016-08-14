;;; ui-config.el ---

;; color theme
(require 'vibrant-ink-inspired)
(setq color-theme-is-global t)


;; Mode line
(require 'spaceline-config)
(setq spaceline-process-p nil)
(setq spaceline-minor-modes-p nil)
(setq spaceline-buffer-encoding-abbrev-p nil)
(spaceline--theme
 '(buffer-modified :face highlight-face)
 '(buffer-id :face highlight-face))
(color-theme-vibrant-ink-inspired)


;; Line highlight
(global-hl-line-mode 1)
(set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)


;; tabs
(setq tab-width 2)
(setq-default indent-tabs-mode nil)


;; search
;; Always end searches at the beginning of the matching expression.
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)
;; Change isearch so that it will leave point at the beginning of the
;; word you've searched for.
(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when isearch-forward (goto-char isearch-other-end)))


;; Turn off UI element
(setq ring-bell-function 'ignore)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq inhibit-startup-message t)  ;; No splash screen


;; Ido
(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
(require 'smex)
(smex-initialize)


;; Tweak
(column-number-mode 1)
(global-font-lock-mode 1)
(show-paren-mode 1)
(set-default 'truncate-lines t)
(setq show-trailing-whitespace t)
(cua-mode 0)
;; highlight marked region
(transient-mark-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)  ;; y/n for yes/no
(require 'smooth-scrolling)
(setq flyspell-issue-message-flag nil)
(put 'narrow-to-region 'disabled nil)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq write-region-inhibit-fsync t)
(global-auto-revert-mode)
;; fucking hate vc-find-file-hook
(remove-hook 'find-file-hook 'vc-find-file-hook)
(require 'ffap)

;; Automatically beblances windows
(defadvice delete-window (after rebalance-d nil activate)
  "Calls `balance-windows' after deleting a window."
  (when (interactive-p)
    (balance-windows)))

(defadvice split-window-vertically (after rebalance-h nil activate)
  "Calls `balance-windows' after splitting a window."
  (when (interactive-p)
    (balance-windows)))

(defadvice split-window-below (after rebalance-h nil activate)
  "Calls `balance-windows' after splitting a window."
  (when (interactive-p)
    (balance-windows)))

(defadvice split-window-horizontally (after rebalance-h nil activate)
  "Calls `balance-windows' after splitting a window."
  (when (interactive-p)
    (balance-windows)))

(defadvice split-window-right (after rebalance-h nil activate)
  "Calls `balance-windows' after splitting a window."
  (when (interactive-p)
    (balance-windows)))


;; Make dabbrev mode more convinient.
(setq dabbrev--eliminate-newlines nil)
(setq dabbrev-case-distinction nil)
(setq dabbrev-case-fold-search t)
(setq dabbrev-case-replace nil)
(setq dabbrev-ignored-buffer-names nil)
(setq dabbrev-upcase-means-case-search nil)


;; Make some buffers only pop up specific places.
(defun my-display-completions (buf)
  "put the *completions* buffer in the right spot"
  (save-selected-window
    (let ((top-right (window-at (- (frame-width) 2) 0))
          (bottom-right (window-at (- (frame-width) 2) (- (frame-height) 2))))
      (if (eq top-right bottom-right)
          (progn
            (select-window top-right)
            (split-window-vertically)))
      (let ((target-window (window-at (- (frame-width) 2) (- (frame-height) 2))))
        (set-window-buffer target-window buf)
        target-window))))

(add-to-list 'special-display-buffer-names '("*Completions*" my-display-completions))
(add-to-list 'special-display-buffer-names '("*Help*" my-display-completions))
(add-to-list 'special-display-buffer-names '("*compilation*" my-display-completions))


;; recnetf
(require 'recentf)
(recentf-mode 1)
(run-at-time nil (* 12 3600) 'recentf-save-list)


;; backup
(defconst use-backup-dir t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/.backup")))
      version-control t                ; Use version numbers for backups
      kept-new-versions 16             ; Number of newest versions to keep
      kept-old-versions 2              ; Number of oldest versions to keepev
      delete-old-versions t            ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ; Copy linked files, don't rename.
(setq backup-inhibited t)
(setq auto-save-default t)


;; make gc run less often
(setq gc-cons-threshold 1000000000)

(provide 'ui-config)
