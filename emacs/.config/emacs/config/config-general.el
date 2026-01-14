;;; config-general.el --- general.el configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configures general.el package to provide leader keymaps.

;;; Code:

(use-package which-key
  :ensure nil
  :init
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.3))

(use-package general
  :after evil
  :config
  (general-evil-setup)

  (general-create-definer custom/leader-keys
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-m")

  (general-define-key
   :states 'normal
   "C--" 'text-scale-decrease
   "C-=" 'text-scale-increase)

  (custom/leader-keys
    "b" '(:ignore t :which-key "buffer")
    "c" '(:ignore t :which-key "code")
    "g" '(:ignore t :which-key "git")
    "p" '(:ignore t :which-key "project")
    "s" '(:ignore t :which-key "search")
    "." '(find-file :which-key "navigate files")
    "=" '(indent-region :which-key "indent region")
    "t" '(:ignore t :which-key "tab")
    "tn" '(tab-bar-new-tab :which-key "new tab")
    "tc" '(tab-bar-close-tab :which-key "close tab")
    "tr" '(tab-rename :which-key "rename tab")
    "tu" '(tab-undo :which-key "undo closing tab")))

(provide 'config-general)

;;; config-general.el ends here
