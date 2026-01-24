;;; early-init.el --- Early init -*- lexical-binding: t; -*-

;;; Commentary:
;;; Package initialization.
;;; This file is loaded before the package system and GUI is initialized.

;;; Code:

(require 'package)

(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)
(setq package-native-compile t)

;;; early-init.el ends here
