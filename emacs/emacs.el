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

;; Custom made by the UI.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-vibrant)))
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
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
 '(package-selected-packages (quote (company-shell)))
 '(show-paren-mode t)
 '(transient-mark-mode t)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line-buffer-id ((t (:weight bold))))
 '(powerline-active1 ((t (:inherit mode-line :background "grey22" :foreground "color-39"))))
 '(powerline-inactive1 ((t (:inherit mode-line :background "black" :foreground "#a8a8a8"))))
 '(powerline-inactive2 ((t (:inherit mode-line :background "grey22" :foreground "#a8a8a8")))))

(require 'bootstrap)
