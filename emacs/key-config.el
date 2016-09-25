;; ------------------------------ Functions ------------------------------
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defun kill-syntax-forward ()
  "Kill characters with syntax at point."
  (interactive)
  (kill-region (point)
               (progn (skip-syntax-forward (string (char-syntax (char-after))))
                      (point))))

(defun kill-syntax-backward ()
  "Kill characters with syntax at point."
  (interactive)
  (kill-region (point)
               (progn (skip-syntax-backward (string (char-syntax (char-before))))
                      (point))))

(defun revert-no-confirm ()
  (interactive)
  (revert-buffer nil t))

(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
 	   (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
 	   (line-beginning-position 2)))))

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun sudo-edit ()
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
	     buffer-file-name))))

(defun global-set-sticky-key (string func)
  (define-key sticky-map string func))

(defvar rotate-exts '("-defs.h" "-inl.h" "Test.cpp" "_test.cpp" ".h" ".cpp" ".hpp" ".tcc"))

(defun file-ends-with (filename suffix)
  (if (string-match (concat "^\\(.*\\)" suffix "$") filename)
      (match-string 1 filename)))

(defun do-rotate (base exts)
  (if (not exts) nil
    (let ((filename (concat base (car exts))))
      (if (file-exists-p filename) (find-file filename)
        (do-rotate base (cdr exts))))))

(defun do-rotate-among-files (filename to-test tested)
  (let ((ext (car to-test)))
    (if (not ext) nil
      (let ((base (file-ends-with filename ext)))
        (if base ;;base)))))
            (do-rotate base (append (cdr to-test) tested))
          (do-rotate-among-files filename (cdr to-test) (append tested (list ext))))))))

(defun rotate-among-files ()
  (interactive)
  (let* ((filename (buffer-file-name)))
    (if filename (do-rotate-among-files filename rotate-exts '()) nil)))

(defun sc-join-line ()
  (interactive)
  (delete-indentation)
  (delete-horizontal-space))

(defalias 'sl 'sort-lines)

(defun cycle-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defvar last-win nil)
(defadvice select-window (before my-select-window (WINDOW &optional NORECORD) activate)
  "record the previous window"
  (if (not (eq (selected-window) WINDOW))
      (setq last-win (selected-window))))

(defun cycle-window()
  (interactive)
  (let ((w last-win))
    (select-window w)))

(defun toggle-line-wrap ()
  (interactive)
  (setq truncate-lines (not truncate-lines)))

;; Disable minimizing in x11
(global-unset-key (kbd "C-z"))

(provide 'key-config)
