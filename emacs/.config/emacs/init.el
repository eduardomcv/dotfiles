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
(add-to-list 'default-frame-alist '(font . "JetBrainsMono-14"))

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
(setq-default display-line-numbers-type 'relative)
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-grow-only t)
(global-display-line-numbers-mode 1)

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
  (setq vertico-posframe-width 90)
  (setq vertico-posframe-height 25))

;; Rich annotations
(use-package marginalia
  :init
  (marginalia-mode))

;; Highlight TODO comments
(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
         (text-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":")
  ;; Keywords
  (setq hl-todo-keyword-faces
	'(
          ("TODO"     . "#96CDFB")
	  ("FIXME"       . "#F28FAD")
          ("HACK"       . "#FAE3B0")
          ("DEPRECATED"      . "#E8A2AF")
          ("NOTE"       . "#ABE9B3"))))


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

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode 1))

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
    (setq consult-fd-args (concat fd-name " --full-path --absolute-path --color=never --hidden --exclude .git"))))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult-todo
  :config
  (defconst consult-todo--narrow
    '((?t . "TODO")
      (?f . "FIXME")
      (?h . "HACK")
      (?d . "DEPRECATED")
      (?n . "NOTE"))))

;;; Project detection

(use-package project
  :config
  (setq project-list-file (locate-user-emacs-file "projects"))
  (setq project-switch-commands 'project-find-file))

;;; Formatting
(use-package apheleia
  :config
  (apheleia-global-mode +1))

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
    "c"  '(:ignore t :which-key "code")
    "g"  '(:ignore t :which-key "git")
    "p"  '(:ignore t :which-key "project")
    "b"  '(:ignore t :which-key "buffer")
    "."  '(find-file :which-key "navigate files")
    "bf" '(apheleia-format-buffer :which-key "format buffer")
    "bt" '(consult-todo :which-key "search buffer todos")
    "bl" '(consult-line :which-key "search buffer lines")
    "cf" '(apheleia-format-buffer :which-key "format buffer")
    "gg" '(magit-status :which-key "magit status")
    "pp" '(project-switch-project :which-key "switch project")
    "pb" '(consult-project-buffer :which-key "project buffers")
    "sg" '(consult-ripgrep :which-key "grep")
    "sf" '(project-find-file :which-key "project files")
    "sb" '(consult-buffer :which-key "buffers")
    "ss" '(consult-line :which-key "current file")
    "st" '(consult-todo-project :which-key "project todos")))
