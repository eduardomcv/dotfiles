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
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
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

;; Eldoc child frame
(use-package eldoc-box
  :hook ((eglot-managed-mode . eldoc-box-hover-mode)
         (eglot-managed-mode . eldoc-box-hover-at-point-mode))
  :init
  (setq eldoc-idle-delay 0.1)
  :config
  (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t)
  (set-face-attribute 'eldoc-box-border nil :background  "#89b4fa")
  (set-face-attribute 'eldoc-box-body nil :background "#1e1e2e" :family "JetBrainsMono" :height 130)
  (setq eldoc-box-clear-with-C-g t)
  (setq eldoc-box-frame-parameters
        '(
	  (left-fringe . 8)
          (right-fringe . 8)
	  (internal-border-width . 1)
	  (vertical-scroll-bars . nil)
	  (horizontal-scroll-bars . nil)
	  (menu-bar-lines . 0)
	  (tool-bar-lines . 0)
          (line-spacing . 0.1)
	  (cursor-type . nil))))

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

;;; Completion

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)
  :custom-face
  (corfu-default ((t (:background "#1e1e2e" :foreground "#cdd6f4"))))
  (corfu-border ((t (:background "#89b4fa"))))
  (corfu-current ((t (:background "#313244" :foreground "#cdd6f4" :weight bold))))
  (corfu-bar ((t (:background "#585b70")))))

(use-package emacs
  :init
  (setq tab-always-indent 'indent))

;;; LSP
(use-package eglot
  :hook
  ((prog-mode . eglot-ensure))
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eglot-ignored-server-capabilities '(:hoverProvider))
  (add-to-list 'eglot-stay-out-of 'flymake))

;;; Git

;; Magit
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Git signs
(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode)
  :config
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  (setq diff-hl-margin-symbols-alist
        '((insert . "│")
          (delete . "│")
          (change . "│")
          (unknown . "│")
          (ignored . "│")))

  (custom-set-faces
   '(diff-hl-insert ((t (:foreground "#a6e3a1" :background nil))))
   '(diff-hl-delete ((t (:foreground "#f38ba8" :background nil))))
   '(diff-hl-change ((t (:foreground "#89b4fa" :background nil))))))

;;; Fuzzy finder
(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
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

;;; Linting

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config
  ;; 1. keybindings are already set in your General section ([d / ]d)
  
  ;; 2. Customize the "Signs" (Gutter Icons)
  ;; Define bitmaps for dots/arrows
  (define-fringe-bitmap 'flymake-fringe-bitmap-circle
    (vector #b00000000
            #b00111100
            #b01111110
            #b01111110
            #b01111110
            #b01111110
            #b00111100
            #b00000000))

  ;; Map Errors/Warnings to these bitmaps
  (setq flymake-error-bitmap '(flymake-fringe-bitmap-circle compilation-error))
  (setq flymake-warning-bitmap '(flymake-fringe-bitmap-circle compilation-warning))
  (setq flymake-note-bitmap '(flymake-fringe-bitmap-circle compilation-info))

  (custom-set-faces
   '(flymake-error ((t (:underline (:style wave :color "#F28FAD")))))
   '(flymake-warning ((t (:underline (:style wave :color "#FAE3B0")))))
   '(flymake-note ((t (:underline (:style wave :color "#96CDFB")))))))

(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup))

;;; Key bindings

(use-package general
  :after evil
  :config
  (general-evil-setup)

  (general-create-definer leader-def
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-m")

  (general-define-key
   :keymaps 'corfu-map
   "TAB" nil
   "<tab>" nil
   "C-i" nil
   "C-j" 'corfu-next
   "C-k" 'corfu-previous
   "C-SPC" 'corfu-insert)

  (general-define-key
   :states 'normal
   "[d"  'flymake-goto-prev-error
   "]d"  'flymake-goto-next-error
   "gD"  'eglot-find-declaration
   "gI"  'eglot-find-implementation
   "K"   'eldoc-box-help-at-point
   "C-p" 'project-find-file)

  (general-define-key
   :states 'insert
   "C-SPC" 'completion-at-point
   )

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
    "ca" '(eglot-code-actions :which-key "actions")
    "cr" '(eglot-rename :which-key "rename")
    "ch" '(eldoc :which-key "hover documentation")
    "gg" '(magit-status :which-key "magit status")
    "pp" '(project-switch-project :which-key "switch project")
    "pb" '(consult-project-buffer :which-key "project buffers")
    "sg" '(consult-ripgrep :which-key "grep")
    "sf" '(project-find-file :which-key "project files")
    "sb" '(consult-buffer :which-key "buffers")
    "ss" '(consult-line :which-key "current file")
    "st" '(consult-todo-project :which-key "project todos")))

