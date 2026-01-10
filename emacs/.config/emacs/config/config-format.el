;;; config-format.el --- Formatting -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure apheleia package for formatting.

;;; Code:

(use-package apheleia
  :custom
  (apheleia-formatters-respect-indent-level nil)

  :config
  ;; Replace default (black) to use ruff for sorting import and formatting.
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))

  (apheleia-global-mode 1))

(provide 'config-format)

;;; config-format.el ends here
