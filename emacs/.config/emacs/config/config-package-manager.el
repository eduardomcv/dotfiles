;;; config-package-manager.el --- Package manager configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure package manager, use-package and MELPA repository.

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)
(setq package-native-compile t)

(provide 'config-package-manager)

;;; config-package-manager.el ends here
