;;; config-general.el --- general.el configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configures general.el package to provide leader keymaps.

;;; Code:

(use-package general
  :after evil
  :config
  (general-evil-setup)

  (general-create-definer custom/leader-keys
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-m")

  ;; Normal mode
  (general-define-key
   :states 'normal
   "[d"  'flymake-goto-prev-error
   "]d"  'flymake-goto-next-error
   "gD"  'eglot-find-declaration
   "gI"  'eglot-find-implementation
   "C-p" 'project-find-file
   "C--" 'text-scale-decrease
   "C-=" 'text-scale-increase)

  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   "K" 'custom/toggle-eldoc-tooltip)

  ;; Insert mode
  (general-define-key
   :states 'insert
   ;; Trigger completion with C-SPC
   "C-SPC" 'completion-at-point)

  ;; Dired
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "TAB" 'dired-toggle-read-only)

  ;; WDired
  (general-define-key
   :states 'normal
   :keymaps 'wdired-mode-map
   "=" 'wdired-finish-edit
   "ESC" 'wdired-abort-changes)

  (custom/leader-keys
    ;; Search
    "s"   '(:ignore t :which-key "search")
    "sg"  '(consult-ripgrep :which-key "grep")
    "sf"  '(project-find-file :which-key "project files")
    "sb"  '(consult-buffer :which-key "buffers")
    "ss"  '(consult-line :which-key "current file")
    "sr"  '(consult-recent-file :which-key "recent files")
    "st"  '(consult-todo-project :which-key "project todos")

    ;; Code
    "c"   '(:ignore t :which-key "code")
    "cf"  '(apheleia-format-buffer :which-key "format buffer")
    "ca"  '(eglot-code-actions :which-key "actions")
    "cr"  '(eglot-rename :which-key "rename")
    "cw"  '(whitespace-mode :which-key "toggle whitespace")
    "ch"  '(eldoc :which-key "hover documentation")

    ;; Git
    "g"   '(:ignore t :which-key "git")
    "gg"  '(magit-status :which-key "magit status")

    ;; Project
    "p"   '(:ignore t :which-key "project")
    "pp"  '(project-switch-project :which-key "switch project")
    "pb"  '(consult-project-buffer :which-key "project buffers")

    ;; Buffer
    "b"   '(:ignore t :which-key "buffer")
    "bf"  '(apheleia-format-buffer :which-key "format buffer")
    "bt"  '(consult-todo :which-key "search buffer todos")
    "bl"  '(consult-line :which-key "search buffer lines")

    ;; Other
    "."   '(find-file :which-key "navigate files")
    "RET" '(vterm-toggle :which-key "toggle vterm")
    "="   '(indent-region :which-key "indent region")))

(provide 'config-general)

;;; config-general.el ends here
