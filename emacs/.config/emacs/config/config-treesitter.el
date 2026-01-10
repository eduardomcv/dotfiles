;;; config-treesitter.el --- Tree-sitter configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure tree-sitter and automatically install language grammars.

;;; Code:

(use-package treesit
  :ensure nil
	:custom
  (treesit-font-lock-level 4))

(use-package treesit-auto
  :custom
  (treesit-auto-install t)
	(treesit-auto-fallback-to-standard-mode t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)

  (global-treesit-auto-mode))

(provide 'config-treesitter)

;;; config-treesitter.el ends here
