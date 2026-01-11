;;; config-terminal.el --- Terminal configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure terminal emulation in Emacs using vterm.

;;; Code:

(use-package vterm
  :custom
  (vterm-shell (or (executable-find "zsh") shell-file-name))
  (vterm-max-scrollback 10000)

  :config
  (setq vterm-timer-delay 0.01))

(use-package vterm-toggle
  :after vterm
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-scope 'project)
  :config
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (dedicated . t)
                 (reusable-frames . visible)
                 (window-height . 0.3)))
  :general
  (custom/leader-keys
    "RET" '(vterm-toggle :which-key "toggle vterm")))

(provide 'config-terminal)

;;; config-terminal.el ends here
