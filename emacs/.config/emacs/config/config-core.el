;;; config-core.el --- Core configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configuration for core functionality, such as package manager, base Emacs configuration,
;;; garbage collector, shell injection, etc.

;;; Code:

(use-package
 gcmh
 :init (gcmh-mode 1)
 :custom
 (gcmh-idle-delay 'auto)
 (gcmh-auto-idle-delay-factor 10)
 (gcmh-high-cons-threshold (* 128 1024 1024)))

(use-package
 no-littering
 :config
 (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
 (no-littering-theme-backups)
 (when (file-exists-p custom-file)
   (load custom-file 'noerror)))

(use-package
 exec-path-from-shell
 :config
 ;; GUI Emacs doesn't inherit the shell's PATH.
 (add-to-list 'exec-path-from-shell-variables "TERMINFO")
 (exec-path-from-shell-initialize)
 ;; Fallback for sessions that skip the login shell (e.g. a daemon).
 ;; Optional: mason.el's runtimes still resolve without mise installed.
 (let ((mise-shims (expand-file-name "~/.local/share/mise/shims")))
   (when (file-directory-p mise-shims)
     (add-to-list 'exec-path mise-shims)
     (unless (member mise-shims (split-string (getenv "PATH") path-separator))
       (setenv "PATH" (concat mise-shims path-separator (getenv "PATH")))))))

;; Set here, not in config-bootstrap.el, since config-code.el (loads
;; earlier) needs it to point dap-mode at mason's debugpy/js-debug-adapter.
(use-package
 mason
 :init (setq mason-dir (no-littering-expand-var-file-name "mason/")))

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
 ;; Skip tree-sitter/font-lock on very large files
 (global-so-long-mode 1)

 :custom
 (inhibit-startup-message t)
 (read-process-output-max (* 1024 1024))
 (version-control t)
 (delete-old-versions t)
 (kept-new-versions 6)
 (kept-old-versions 2)
 (backup-by-copying t)
 (delete-by-moving-to-trash t)
 (global-auto-revert-non-file-buffers t)
 (browse-url-browser-function 'browse-url-default-browser)
 (use-short-answers t)
 (visible-bell nil)
 (ring-bell-function 'ignore)
 (scroll-conservatively 101)
 (scroll-margin 8)
 (mouse-wheel-progressive-speed nil)
 (mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control) . 6)))

 :config
 (setq-default indent-tabs-mode nil)
 (setq-default tab-width 2)

 (when (eq system-type 'darwin)
   (require 'server)
   (unless (server-running-p)
     (server-start))))

(provide 'config-core)

;;; config-core.el ends here
