;;; config-terminal.el --- Terminal configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure terminal emulation in Emacs using vterm and multi-vterm.

;;; Code:

(use-package vterm
  :custom
  (vterm-shell (or (executable-find "zsh") shell-file-name))
  (vterm-max-scrollback 10000)
  (vterm-timer-delay 0.01))

(use-package multi-vterm
  :ensure t
  :after vterm
  :custom
  (multi-vterm-dedicated-window-height-percent 30)
  :bind
  (:map vterm-mode-map
        ("C-n" . multi-vterm-next)
        ("C-p" . multi-vterm-prev))
  :general
  (custom/leader-keys
    "tt" '(multi-vterm-project :which-key "toggle project terminal")
    "pt" '(multi-vterm-project :which-key "toggle project terminal")
    "tn" '(multi-vterm :which-key "new terminal"))
  (:states '(normal insert)
   "M-RET" 'multi-vterm))

(provide 'config-terminal)

;;; config-terminal.el ends here
