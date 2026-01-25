;;; config-core.el --- Core configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configuration for core functionality, such as package manager, base Emacs configuration,
;;; garbage collector, shell injection, etc.

;;; Code:

(use-package diminish)

(use-package
 gcmh
 :diminish gcmh-mode
 :init (gcmh-mode 1)
 :custom
 (gcmh-idle-delay 'auto)
 (gcmh-auto-idle-delay-factor 10)
 (gcmh-high-cons-threshold (* 128 1024 1024)))

(use-package
 no-littering
 :config
 (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
 (setq backup-directory-alist
       `(("." . ,(no-littering-expand-var-file-name "backup/"))))
 (when (file-exists-p custom-file)
   (load custom-file 'noerror)))

(use-package
 exec-path-from-shell
 :if (or (daemonp) (memq window-system '(mac ns x)))
 :config (exec-path-from-shell-initialize))

(use-package
 emacs
 :ensure nil
 :init
 (global-auto-revert-mode 1)
 (save-place-mode 1)
 (electric-pair-mode 1)
 (recentf-mode 1)
 (context-menu-mode 1)
 (winner-mode 1)

 :hook (emacs-lisp-mode . (lambda () (setq tab-width 2)))

 :custom (read-process-output-max (* 1024 1024))

 (version-control t)
 (delete-old-versions t)
 (kept-new-versions 6)
 (kept-old-versions 2)

 (global-auto-revert-non-file-buffers t)

 (browse-url-browser-function 'browse-url-default-browser)

 (use-short-answers t)

 (visible-bell nil)
 (ring-bell-function 'ignore)

 (delete-by-moving-to-trash t)

 (scroll-conservatively 101)
 (mouse-wheel-progressive-speed nil)
 (mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control) . 6)))

 :config
 (setq-default indent-tabs-mode nil)
 (setq-default tab-width 4)

 (run-at-time nil 300 'recentf-save-list))

(provide 'config-core)

;;; config-core.el ends here
