;; -- Init package paths -----------------------------------

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.dotfiles/emacs/")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

(require 'bootstrap)
(require 'ui-config)
(require 'coding-config)
(require 'key-config)
(require 'local-config)

;; Custom made by the UI.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(fill-column 80)
 '(global-pabbrev-mode t)
 '(hippie-expand-try-functions-list
   (quote
    (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol try-expand-line try-expand-whole-kill)))
 '(pabbrev-global-mode-buffer-size-limit nil)
 '(pabbrev-idle-timer-verbose nil)
 '(pabbrev-marker-distance-before-scavenge 2000)
 '(pabbrev-minimal-expansion-p t)
 '(pabbrev-scavenge-some-chunk-size 40)
 '(pabbrev-thing-at-point-constituent (quote symbol))
 '(package-selected-packages
   (quote
    (zzz-to-char rtags yatemplate wgrep company commpany use-package
                 (use-package)
                 flx counsel ivy window-numbering window-number undo-tree spaceline smooth-scrolling smex moe-theme markdown-mode magit-find-file keyfreq ido-vertical-mode find-file-in-repository find-file-in-project color-theme buffer-stack ample-theme ace-window)))
 '(show-paren-mode t)
 '(transient-mark-mode t)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(powerline-active1 ((t (:inherit mode-line :background "grey22" :foreground "color-39")))))
