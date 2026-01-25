;;; early-init.el --- Early init -*- lexical-binding: t; -*-

;;; Commentary:
;;; Package initialization.
;;; This file is loaded before the package system and GUI is initialized.

;;; Code:

(setq native-comp-async-report-warnings-errors 'silent)
(setq byte-compile-warnings
      '(not free-vars unresolved noruntime lexical make-local))

(if noninteractive
    (setq
     gc-cons-threshold 134217728 ; 128mb
     gc-cons-percentage 1.0)
  ;; Later, this value is reset by gcmh
  (setq gc-cons-threshold most-positive-fixnum))

(setq package-native-compile t)
(setq use-package-always-ensure t)

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

;;; early-init.el ends here
