;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Adds the modules in the "config" directory and handles load order.

;;; Code:

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'config-core)
(require 'config-ui)
(require 'config-editor)
(require 'config-code)
(require 'config-lsp)
(require 'config-format)
(require 'config-lint)
(require 'config-git)
(require 'config-finder)
(require 'config-org)
(require 'config-terminal)

;;; init.el ends here
