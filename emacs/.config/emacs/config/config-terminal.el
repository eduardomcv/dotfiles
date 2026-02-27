;;; config-terminal.el --- Terminal configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure terminal emulation in Emacs using vterm and multi-vterm.

;;; Code:

(use-package
 vterm

 :custom
 (vterm-shell (or (executable-find "zsh") shell-file-name))
 (vterm-max-scrollback 10000)
 (vterm-timer-delay 0.01)
 (vterm-kill-buffer-on-exit t)

 :hook
 (vterm-mode
  .
  (lambda ()
    (buffer-face-set 'fixed-pitch)
    (setq-local line-spacing 0)
    (display-line-numbers-mode -1)
    (hl-line-mode -1)))

 :config (define-key vterm-mode-map [return] #'vterm-send-return)

 (defun custom/spawn-shell (name)
   "Create a new vterm buffer called NAME."
   (interactive "sName of shell: ")
   ;; Format buffer name to look like an Emacs buffer (*name*).
   (let ((buffer-name
          (if (string-prefix-p "*" name)
              name
            (format "*%s*" name))))
     (vterm buffer-name)
     (setq-local vterm-buffer-name-string nil)))

 :general
 (custom/leader-key
  "RET" '(custom/spawn-shell :which-key "spawn a new shell")))

(use-package multi-vterm :after vterm)

(provide 'config-terminal)

;;; config-terminal.el ends here
