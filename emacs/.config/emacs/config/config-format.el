;;; config-format.el --- Formatting -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure apheleia package for formatting.

;;; Code:

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
  :general
  (custom/leader-key
    "cf" '(apheleia-format-buffer :which-key "format buffer")
    "bf" '(apheleia-format-buffer :which-key "format buffer")))

(provide 'config-format)

;;; config-format.el ends here
