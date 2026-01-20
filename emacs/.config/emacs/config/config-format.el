;;; config-format.el --- Formatting -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure apheleia package for formatting.

;;; Code:

(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

(use-package apheleia
  :init
  (apheleia-global-mode 1)
  :custom
  (apheleia-formatters-respect-indent-level nil)
  :config
  ;; Replace default (black) to use ruff for sorting import and formatting.
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))

  (add-to-list 'apheleia-formatters
               '(elisp-autofmt . ("elisp-autofmt")))

  (add-to-list 'apheleia-mode-alist
               '(emacs-lisp-mode . elisp-autofmt))

  :general
  (custom/leader-key
    "cf" '(apheleia-format-buffer :which-key "format buffer")
    "bf" '(apheleia-format-buffer :which-key "format buffer")))

(provide 'config-format)

;;; config-format.el ends here
