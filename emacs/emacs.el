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
 '(custom-enabled-themes (quote (sanityinc-tomorrow-vibrant)))
 '(package-selected-packages
   (quote
    (js3-mode flycheck-flow flycheck company-flow web-mode spacemacs-theme cmake-mode org-bullets company-shell))))

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
