;;; flycheck-hh --- Summary
;;; hh flowcheck checker.

;;; Commentary:
;;; hh flowcheck checker.

;;; Code:

(require 'flycheck)

(flycheck-def-args-var flycheck-hh-args hh)
(customize-set-variable 'flycheck-hh-args '())

(flycheck-define-checker hh
    "A JavaScript syntax and style checker using hack."
    :command ("hh"
              "check"
              (eval flycheck-hh-args)
              source-original)
    :standard-input t
    :predicate
    (lambda ()
      (and
       buffer-file-name
       (file-exists-p buffer-file-name)
       (locate-dominating-file buffer-file-name ".hhconfig")))
    :error-patterns
    ((error line-start
            (file-name)
            ":"
            line
            ":"
            column
            ","
            (one-or-more digit)
            ": "
            (message (minimal-match (and (one-or-more anything) "\n")))
            line-end))
    :modes (xhp-mode))

(add-to-list 'flycheck-checkers 'hh)

(provide 'flycheck-hh)
;;; flycheck-hh.el ends here
