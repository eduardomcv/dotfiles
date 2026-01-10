;;; config-code.el --- Language configurations and utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;; Language configurations and utilities

;;; Code:

(use-package pyvenv
  :custom
  (pyvenv-default-virtual-env-name ".venv")
  :config
  (pyvenv-mode 1))

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")
  :custom
  (markdown-fontify-code-blocks-natively t))

(provide 'config-code)

;;; config-code.el ends here
