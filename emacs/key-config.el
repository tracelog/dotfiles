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

(defun move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (when (null n)
    (setq n 1))
  (let (col (current-column))
    (beginning-of-line)
    (next-line 1)
    (transpose-lines n)
    (previous-line 1)
    (forward-char col)))

(defun move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (when isearch-forward (goto-char isearch-other-end)))

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

(defun vi-open-next-line (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

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

(setq sticky-map (make-sparse-keymap))
(setq emulation-mode-map-alists (list (list (cons 'sticky-map sticky-map))))

(defun global-set-sticky-key (string func)
  (define-key sticky-map string func))

(defvar rotate-exts '("-defs.h" "-inl.h" "Test.cpp" "_test.cpp" ".h" ".cpp" ".hpp"))

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

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
            (find-file file))))

(defun sc-join-line ()
  (interactive)
  (delete-indentation)
  (delete-horizontal-space))

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

(loop for i from 1 to 5 do
     (global-set-key (read-kbd-macro (format "M-%d" i)) 'switch-to-shell))

(defalias 'sl 'sort-lines)

(defun cycle-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))


;; Disable minimizing in x11
(global-unset-key (kbd "C-z"))

(global-set-sticky-key (kbd "C-j") 'next-line)
(global-set-sticky-key (kbd "C-k") 'previous-line)
(global-set-sticky-key (kbd "C-l") 'forward-char)
(global-set-sticky-key (kbd "C-h") 'backward-char)
(global-set-sticky-key (kbd "M-l") 'forward-word)
(global-set-sticky-key (kbd "M-h") 'backward-word)
(global-set-sticky-key (kbd "M-j") 'scroll-up)
(global-set-sticky-key (kbd "M-k") 'scroll-down)
(global-set-sticky-key (kbd "C-o") 'other-window)
(global-set-sticky-key (kbd "M-d") 'kill-line)

(global-set-key (kbd "M-8") (quote match-paren))
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-q") 'kill-syntax-backward)
(global-set-key (kbd "C-d") 'kill-syntax-forward)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)
(global-set-key (kbd "C-x m") 'smex)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-o") 'find-file-at-point)
(global-set-key (kbd "M-t") 'recentf-ido-find-file)
(global-set-key (kbd "C-c l") 'sort-lines)

(global-set-key (kbd "C-x C-g") 'keyboard-quit)

(global-set-key (kbd "<f11>") 'revert-no-confirm)
(global-set-key (kbd "<f12>") 'cycle-buffer)
(global-set-key (kbd "<f6>") 'next-error)
(global-set-key (kbd "<f9>") 'repeat-complex-command)

(global-set-key (kbd "M-=") 'revert-no-confirm)
(global-set-key (kbd "M-i") 'indent-region)
(global-set-key (kbd "M-%") (quote query-replace-regexp))
(global-set-key (kbd "<f3>") (quote query-replace-regexp))
(global-set-key (kbd "ESC <up>") (quote move-line-up))
(global-set-key (kbd "ESC <down>") (quote move-line-down))
(global-set-key (kbd "C-c k") 'kill-line)
(global-set-key (kbd "C-c C-k") 'kill-line)
(global-set-key (kbd "C-c j") 'sc-join-line)
(global-set-key (kbd "C-c C-j") 'sc-join-line)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
(global-set-key (kbd "C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-v") 'ido-switch-buffer)
(global-set-key (kbd "M-e") 'ido-switch-buffer)
(global-set-key (kbd "M-DEL") 'sc-join-line)

(global-set-key (kbd "M-u") 'rename-uniquely)
(global-set-key (kbd "M-RET") (quote vi-open-next-line))

(global-set-key (kbd "C-c C-g") (quote magit-status))
(global-set-key (kbd "C-c C-r") (quote rotate-among-files))
(global-set-key (kbd "C-c r") (quote rotate-among-files))

(require 'buffer-stack)
(global-set-key (kbd "ESC <left>") (quote buffer-stack-down))
(global-set-key (kbd "ESC <right>") (quote buffer-stack-up))
(global-set-key (kbd "C-M-O") (quote ace-window))
(global-set-key (kbd "M-`") (quote ace-window))
(provide 'key-config)
