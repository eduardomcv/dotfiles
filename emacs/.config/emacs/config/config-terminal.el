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

(use-package detached
  :init
  (detached-init)
  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type))
  :bind (
         ([remap async-shell-command] . detached-shell-command)
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ([remap detached-open-session] . detached-consult-session)))

(provide 'config-terminal)

;;; config-terminal.el ends here
