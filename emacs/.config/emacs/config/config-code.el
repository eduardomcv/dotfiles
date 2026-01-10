;;; config-code.el --- Language configurations and utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;; Language configurations and utilities

;;; Code:

(use-package pyvenv
  :custom
  (pyvenv-default-virtual-env-name ".venv")
  :config
  (pyvenv-mode 1)

  (defun custom/auto-activate-python-venv ()
    "Activate .venv directory if it exists in the project root."
    (let* ((project-root (locate-dominating-file buffer-file-name ".venv"))
           (venv-path (and project-root (expand-file-name ".venv" project-root))))
      (when (and venv-path (file-directory-p venv-path))
        (pyvenv-activate venv-path))))

  (add-hook 'python-mode-hook #'custom/auto-activate-python-venv)
  (add-hook 'python-ts-mode-hook #'custom/auto-activate-python-venv))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (yas-reload-all))

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
