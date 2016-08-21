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
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; (-uniq package-activated-list)
(setq my-packages '(ace-window buffer-stack color-theme
      find-file-in-project swiper find-file-in-repository
      ido-vertical-mode keyfreq magit-find-file dash magit
      magit-popup async git-commit with-editor smex
      smooth-scrolling spaceline s powerline undo-tree
      window-numbering markdown-mode
      ))

(apply 'ensure-package-installed my-packages)

(server-start nil)

(provide 'bootstrap)
