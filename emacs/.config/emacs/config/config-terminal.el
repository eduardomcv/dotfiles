;;; config-terminal.el --- Terminal configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure terminal emulation in Emacs using vterm and multi-vterm.

;;; Code:

(use-package vterm
  :custom
  (vterm-shell (or (executable-find "zsh") shell-file-name))
  (vterm-max-scrollback 10000)
  (vterm-timer-delay 0.01)
  :config
  (define-key vterm-mode-map [return] #'vterm-send-return)
  :general
  (custom/leader-keys
    "RET" '(vterm :which-key "open terminal")))

(provide 'config-terminal)

;;; config-terminal.el ends here
