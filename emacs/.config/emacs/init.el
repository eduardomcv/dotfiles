;;; -*- lexical-binding: t; -*-

;;; Package manager

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;;; General settings

;; Performance settings
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Backups
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)

;; Automatically update buffers
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Remember last place visited in files
(save-place-mode 1)

;; Remember recently edited files
(recentf-mode 1)

;; Frame default size
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 45))

;; Don't pollute config directory
(use-package no-littering
  :config
  ;; Store custom file in etc/
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file 'noerror))

  ;; Store backups file in var/
  (setq backup-directory-alist
      `(("." . ,(no-littering-expand-var-file-name "backups/"))))
  ;; Store auto-save files in var/
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;;; Theme

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin :no-confirm))

;;; Fonts

(set-face-attribute 'default nil :font "JetBrainsMono" :height 140 :weight 'regular)

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
                              "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                              "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                              "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                              "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                              "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                              "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                              "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                              "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                              "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                              "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                              ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                              "<:<" ";;;"))
  (global-ligature-mode t))

;;; UI

;; Remove menu, toolbar and scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable word wrap
(global-visual-line-mode 1)

;; Highlight current line
(hl-line-mode 1)

;; Consistent relative line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-width 4)
(setq display-line-numbers-grow-only t)

;; Enable which key
(which-key-mode 1)

;; Completion UI
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-resize t)
  (setq vertico-count 15)
  :custom
  (vertico-cycle t))

(use-package vertico-posframe
  :after vertico
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))

  (setq vertico-posframe-border-width 2)

  ;; 3. Position it in the center of the frame
  ;;(setq vertico-posframe-poshandler 'vertico-posframe-poshandler-frame-center)
  
  ;; 4. (Optional) Make it slightly smaller/wider to match your taste
  (setq vertico-posframe-width 80)
  (setq vertico-posframe-height 20))

;; Rich annotations
(use-package marginalia
  :init
  (marginalia-mode))

;;; Evil mode

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (define-key evil-motion-state-map (kbd "SPC") nil))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;;; Git

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;; Fuzzy finder
(use-package consult
  :bind (
    ("C-c g" . consult-ripgrep)
    ("C-c f" . consult-fd)
    ("C-c b" . consult-buffer)
    ("C-c s" . consult-line)
  )
  :config
  (setq consult-async-min-input 0)
  (let ((fd-name (if (executable-find "fdfind") "fdfind" "fd")))
    (setq consult-fd-args (concat fd-name " --full-path --absolute-path --color=never --hidden --exclude .git")))
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC"))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; Key bindings

(use-package general
  :after evil
  :config
  (general-evil-setup)
  (general-create-definer leader-def
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")
  (leader-def
   "s"  '(:ignore t :which-key "search")
   "sg" '(consult-ripgrep :which-key "grep")
   "sf" '(find-file :which-key "files")
   "sb" '(consult-buffer :which-key "buffers")
   "ss" '(consult-line :which-key "current file")
   "g"  '(:ignore t :which-key "git")
   "gg" '(magit-status :which-key "magit status")))
