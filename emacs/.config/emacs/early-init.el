;;; early-init.el --- Early init -*- lexical-binding: t; -*-

;;; Commentary:
;;; Package initialization.
;;; This file is loaded before the package system and GUI is initialized.

;;; Code:

(if noninteractive
    (setq
     gc-cons-threshold (* 128 1024 1024)
     gc-cons-percentage 1.0)
  ;; Later, this value is reset by gcmh
  (setq gc-cons-threshold most-positive-fixnum))

(setq native-comp-async-report-warnings-errors 'silent)

;; Set before the frame is drawn to avoid a flash and implied resize.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(setq frame-inhibit-implied-resize t)
(setq inhibit-splash-screen t)

;; Avoids activating every package twice; `package-initialize' below does it.
(setq package-enable-at-startup nil)
(require 'package)
(setq package-native-compile t)

(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)

(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;; Use plists for deserialization (better performance)
(setenv "LSP_USE_PLISTS" "true")

;;; early-init.el ends here
