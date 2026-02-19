;;; early-init.el --- Early init -*- lexical-binding: t; -*-

;;; Commentary:
;;; Package initialization.
;;; This file is loaded before the package system and GUI is initialized.

;;; Code:

(if noninteractive
    (setq
     gc-cons-threshold 134217728 ; 128mb
     gc-cons-percentage 1.0)
  ;; Later, this value is reset by gcmh
  (setq gc-cons-threshold most-positive-fixnum))

(setq native-comp-async-report-warnings-errors 'silent)
(setq byte-compile-warnings
      '(not free-vars unresolved noruntime lexical make-local))

(require 'package)
(setq package-native-compile t)

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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Use plists for deserialization (better performance)
(setenv "LSP_USE_PLISTS" "true")

;;; early-init.el ends here
