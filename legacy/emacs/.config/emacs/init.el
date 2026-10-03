;;; init.el --- User Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Adds the modules in the "config" directory and handles load order.

;;; Code:

;; Load Omarchy integration (theme syncing, font syncing, file watchers).
;; Remove this line to opt out of Omarchy Emacs integration.
(load (expand-file-name "omarchy" user-emacs-directory))

;; Your customizations below

(add-to-list
 'load-path (expand-file-name "config" user-emacs-directory))

(require 'config-core)
(require 'config-editor)
(require 'config-ui)
(require 'config-code)
(require 'config-git)
(require 'config-finder)
(require 'config-org)
(require 'config-terminal)
(require 'config-ai)
;; Must load last: installs whatever the files above declared but didn't
;; find on this machine.
(require 'config-bootstrap)

;;; init.el ends here
