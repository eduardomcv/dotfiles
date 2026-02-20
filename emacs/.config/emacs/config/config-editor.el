;;; config-editor.el --- Editor configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configurations for the editor experience.

;;; Code:

(use-package
 emacs
 :ensure nil
 :custom
 (tab-always-indent 'complete)
 (text-mode-ispell-word-completion nil))

(use-package
 undo-fu
 :custom
 (undo-limit 67108864)
 (undo-strong-limit 100663296)
 (undo-outer-limit 1006632960))

(use-package
 evil
 :init
 (setq evil-want-integration t)
 (setq evil-want-keybinding nil)
 (setq evil-want-C-u-scroll t)
 (setq evil-undo-system 'undo-fu)
 :config
 (setq evil-echo-state nil)
 (setq evil-mode-line-format nil)
 (setq evil-normal-state-tag " NORMAL ")
 (setq evil-insert-state-tag " INSERT ")
 (setq evil-visual-state-tag " VISUAL ")
 (setq evil-operator-state-tag " OPERATOR ")
 (setq evil-replace-state-tag " REPLACE ")
 (setq evil-motion-state-tag " MOTION ")
 (define-key evil-motion-state-map (kbd "SPC") nil)
 (evil-mode 1))

(use-package
 evil-collection
 :after evil
 :config (evil-collection-init))

(use-package
 evil-commentary
 :after evil
 :config (evil-commentary-mode 1))

(use-package evil-surround :config (global-evil-surround-mode 1))

(use-package
 which-key
 :ensure nil
 :init (which-key-mode 1)
 :custom (which-key-idle-delay 0.3))

(use-package
 general
 :after evil
 :config (general-evil-setup)

 (general-create-definer
  custom/leader-key
  :states '(normal visual insert emacs)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "M-m")

 (general-define-key
  :states
  'normal
  "C--"
  'text-scale-decrease
  "C-="
  'text-scale-increase)

 (custom/leader-key
  "b"
  '(:ignore t :which-key "buffer")
  "c"
  '(:ignore t :which-key "code")
  "g"
  '(:ignore t :which-key "git")
  "p"
  '(:ignore t :which-key "project")
  "s"
  '(:ignore t :which-key "search")
  "="
  '(indent-region :which-key "indent region")
  "h"
  '(evil-window-left :which-key "move cursor to left window")
  "j"
  '(evil-window-down :which-key "move cursor to window below")
  "k"
  '(evil-window-up :which-key "move cursor to window above")
  "l"
  '(evil-window-right :which-key "move cursor to right window")
  "t"
  '(:ignore t :which-key "tab")
  "tn"
  '(tab-bar-new-tab :which-key "new tab")
  "tc"
  '(tab-bar-close-tab :which-key "close tab")
  "tr"
  '(tab-rename :which-key "rename tab")
  "ts"
  '(tab-switch :which-key "switch tab by name")
  "tu"
  '(tab-undo :which-key "undo closing tab")))

(use-package
 compile
 :ensure nil
 :config
 ;; Support ESLint "stylish" format in compile buffers
 (defconst custom/eslint-stylish-regexp
   '(("^\\(/.*\\)$" 1)
     ("^[ \t]+\\([0-9]+\\):\\([0-9]+\\)[ \t]+" nil 1 2)))
 (add-to-list
  'compilation-error-regexp-alist 'custom/eslint-stylish-regexp)
 (add-to-list
  'compilation-error-regexp-alist-alist
  (cons 'custom/eslint-stylish-regexp custom/eslint-stylish-regexp)))

(use-package
 dired
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
 :general
 (custom/leader-key "e" '(dired-jump :which-key "dired"))
 (:states
  'normal
  :keymaps
  'dired-mode-map
  "TAB"
  'dired-toggle-read-only
  "n"
  'dired-create-empty-file
  "N"
  'dired-create-directory
  "h"
  'dired-up-directory
  "l"
  'dired-find-file))

(use-package
 wdired
 :ensure nil
 :general
 (:states
  'normal
  :keymaps
  'wdired-mode-map
  "="
  'wdired-finish-edit
  "ESC"
  'wdired-abort-changes))

(use-package
 evil-collection-dired
 :ensure nil
 :after (dired evil-collection)
 :config (evil-collection-dired-setup))

(use-package diredfl :hook (dired-mode . diredfl-mode))

(use-package
 nerd-icons-dired
 :hook (dired-mode . nerd-icons-dired-mode))

(use-package
 project
 :ensure nil
 :config
 (setq project-switch-commands
       '((project-find-file "Find file" ?f)
         (project-dired "Dired" ?d)
         (consult-project-buffer "Buffer" ?b)
         (consult-ripgrep "Ripgrep" ?g)
         (magit-project-status "Magit" ?G)))

 (defun project-find-go-module (dir)
   (when-let ((root (locate-dominating-file dir "go.mod")))
     (cons 'go-module root)))
 (cl-defmethod project-root ((project (head go-module)))
   (cdr project))

 (add-hook 'project-find-functions #'project-find-go-module)

 :general (:states 'normal "C-p" 'project-find-file)
 (custom/leader-key
  "sf"
  '(project-find-file :which-key "project files")
  "pp"
  '(project-switch-project :which-key "switch project")))

(use-package
 corfu
 :custom
 (corfu-auto t)
 (corfu-auto-prefix 2)
 (corfu-cycle t)
 (corfu-preselect 'prompt)
 (corfu-popupinfo-delay 0.2)
 (corfu-popupinfo-max-height 20)
 :init (global-corfu-mode)
 :config (corfu-popupinfo-mode)
 :general
 (:states 'insert "C-SPC" 'completion-at-point)
 (:keymaps
  'corfu-map
  "M-j"
  #'corfu-popupinfo-scroll-up
  "M-k"
  #'corfu-popupinfo-scroll-down
  "M-d"
  #'corfu-popupinfo-toggle))

(use-package
 kind-icon
 :after corfu
 :custom (kind-icon-default-face 'corfu-default)
 :config
 (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package
 jinx
 :hook (text-mode . jinx-mode)
 :custom (jinx-languages "en_US pt_PT")
 :bind (("M-$" . jinx-correct) ("C-M-$" . jinx-languages))
 :general
 (:states
  'normal
  :keymaps
  'jinx-mode-map
  "[ s"
  'jinx-previous
  "] s"
  'jinx-next
  "z="
  'jinx-correct))

(provide 'config-editor)

;;; config-editor.el ends here
