;;; config-editor.el --- Editor configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configurations related to editing files.
;;; E.g. evil mode, undo, which-key, general keybindings, Dired.

;;; Code:

(use-package undo-fu
  :custom
  (undo-limit 67108864)
  (undo-strong-limit 100663296)
  (undo-outer-limit 1006632960))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (define-key evil-motion-state-map (kbd "SPC") nil))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

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

  (general-create-definer custom/leader-key
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-m")

  (general-define-key
   :states 'normal
   "C--" 'text-scale-decrease
   "C-=" 'text-scale-increase)

  (custom/leader-key
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
    "ts" '(tab-switch :which-key "switch tab by name")
    "tu" '(tab-undo :which-key "undo closing tab")))

(use-package compile
  :ensure nil
  :config
  ;; Support ESLint "stylish" format in compile buffers
  (defconst custom/eslint-stylish-regexp
    '(("^\\(/.*\\)$" 1)
      ("^[ \t]+\\([0-9]+\\):\\([0-9]+\\)[ \t]+"
       nil 1 2)))
  (add-to-list 'compilation-error-regexp-alist 'custom/eslint-stylish-regexp)
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'custom/eslint-stylish-regexp custom/eslint-stylish-regexp)))

(use-package dired
  :ensure nil
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-mouse-drag-files t)
  (dired-listing-switches
   (if (eq system-type 'darwin)
       "-lhA"
     "-lhA --group-directories-first"))
  :config
  (with-eval-after-load 'dired
    (require 'evil-collection-dired)
    (evil-collection-dired-setup))
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "TAB" 'dired-toggle-read-only
   "n"   'dired-create-empty-file
   "N"   'dired-create-directory
   "h"   'dired-up-directory
   "l"   'dired-find-file)
  (general-define-key
   :states 'normal
   :keymaps 'wdired-mode-map
   "="   'wdired-finish-edit
   "ESC" 'wdired-abort-changes)
  (custom/leader-key
    "." '(dired-jump :which-key "dired")))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package project
  :ensure nil
  :config
  (setq project-switch-commands
        '((project-find-file "Find file" ?f)
          (project-dired "Dired" ?d)
          (consult-project-buffer "Buffer" ?b)
          (consult-ripgrep "Ripgrep" ?g)
          (magit-project-status "Magit" ?G)))
  :general
  (:states 'normal
           "C-p" 'project-find-file)
  (custom/leader-key
    "sf" '(project-find-file :which-key "project files")
    "pp" '(project-switch-project :which-key "switch project")))

(provide 'config-editor)

;;; config-editor.el ends here
