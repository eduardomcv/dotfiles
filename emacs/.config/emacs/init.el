;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Adds the modules in the "config" directory and handles load order.

;;; Code:

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'config-package-manager)
(require 'config-emacs)
(require 'config-undo)
(require 'config-evil)
(require 'config-general)
(require 'config-ui)
(require 'config-dired)
(require 'config-completion)
(require 'config-code)
(require 'config-treesitter)
(require 'config-lsp)
(require 'config-format)
(require 'config-lint)
(require 'config-git)
(require 'config-finder)
(require 'config-org)
(require 'config-project)
(require 'config-terminal)

;;; init.el ends here
