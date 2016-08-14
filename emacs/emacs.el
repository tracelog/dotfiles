;; -- Init package paths -----------------------------------
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
(smex-update)

;; Custom made by the UI.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "red4" "green4" "yellow4" "DarkSlateGray2" "magenta4" "cyan4" "white"])
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
 '(recentf-max-menu-items 10)
 '(recentf-max-saved-items 1000)
 '(recentf-save-file "~/.emacs.d/recentf")
 '(show-paren-mode t)
 '(transient-mark-mode t)
 '(vc-follow-symlinks t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-A ((nil (:background "red" :foreground "brightyellow"))))
 '(ediff-current-diff-B ((nil (:background "green" :foreground "color-55"))))
 '(ediff-even-diff-A ((((class color) (min-colors 16)) (:background "magenta" :foreground "color-21"))))
 '(ediff-even-diff-B ((((class color) (min-colors 16)) (:background "yellow" :foreground "color-21"))))
 '(ediff-fine-diff-A ((((class color) (min-colors 16)) (:background "red" :foreground "color-195" :weight normal))))
 '(ediff-fine-diff-B ((((class color) (min-colors 16)) (:background "yellow" :foreground "color-27"))))
 '(ediff-odd-diff-A ((((class color) (min-colors 16)) (:background "magenta" :foreground "color-88"))))
 '(ediff-odd-diff-B ((((class color) (min-colors 16)) (:background "yellow" :foreground "color-88"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(powerline-active1 ((t (:inherit mode-line :background "grey22" :foreground "color-39")))))
