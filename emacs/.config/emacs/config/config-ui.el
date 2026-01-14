;;; config-ui.el --- UI configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Adds UI related packages and configurations.

;;; Code:

(use-package emacs
  :ensure nil

  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (pixel-scroll-precision-mode 1)
  (global-visual-line-mode 1)
  (global-hl-line-mode 1)
  (global-display-line-numbers-mode 1)

  :custom
  (pixel-scroll-precision-large-scroll-height 40.0)
  (pixel-scroll-precision-interpolation-factor 1.0)
  (pixel-scroll-precision-interpolate-page t)

  (display-line-numbers-type 'relative)
  (display-line-numbers-width 3)
  (display-line-numbers-grow-only t)

  :config
  (setq-default line-spacing 0.1)

  (add-to-list 'default-frame-alist '(font . "Iosevka-16"))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(width . 120))
  (add-to-list 'default-frame-alist '(height . 45))
  (add-to-list 'default-frame-alist '(alpha-background . 95))

  (defun custom/set-font-faces ()
    (set-face-attribute 'default nil
                        :font "Iosevka"
                        :height 160
                        :weight 'regular)

    (set-face-attribute 'variable-pitch nil
                        :font "Iosevka Aile"
                        :height 160
                        :weight 'regular)

    (set-face-attribute 'fixed-pitch nil
                        :font "Iosevka Term"
                        :height 160
                        :weight 'regular)

    (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
    (set-face-attribute 'font-lock-keyword-face nil :slant 'italic))

  (if (daemonp)
      (add-hook 'after-make-frame-functions (lambda (frame) (with-selected-frame frame (custom/set-font-faces))))
    (custom/set-font-faces)))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup)
  :config
  (setq whitespace-style '(face
                           tabs
                           spaces
                           trailing
                           space-before-tab
                           newline
                           empty
                           indentation))
  :general
  (custom/leader-keys
    "cw"  '(whitespace-mode :which-key "toggle whitespace")))

(use-package nerd-icons)

(use-package ligature
  :init
  (global-ligature-mode t)
  :config
  ;; Specific ligature configuration for the Iosevka font
  (ligature-set-ligatures '(prog-mode text-mode) '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->"
                                                   "<--->" "<---->" "<!--" "<==" "<===" "<=" "=>" "=>>"
                                                   "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                                   "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                                   ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:"
                                                   "=:" "<******>" "++" "+++")))

(use-package catppuccin-theme
  :custom
  (catppuccin-flavor 'mocha)
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :custom
  (vertico-resize t)
  (vertico-count 15)
  (vertico-cycle t))

(use-package vertico-posframe
  :init
  (vertico-posframe-mode 1)
  :custom
  (vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package dashboard
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  :custom
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-display-icons-p t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)

  (dashboard-items '((recents . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)
                     (registers . 5)))

  (dashboard-item-shortcuts '((recents   . "r")
                              (bookmarks . "m")
                              (projects  . "p")
                              (agenda    . "a")
                              (registers . "e")))
  :config
  (set-face-attribute 'dashboard-text-banner nil :slant 'normal)
  (setq dashboard-startup-banner (expand-file-name "banner.txt" user-emacs-directory))

  (dashboard-setup-startup-hook))

(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
         (text-mode . hl-todo-mode))
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
   '(("TODO"     . "#96CDFB")
     ("FIXME"       . "#F28FAD")
     ("HACK"       . "#FAE3B0")
     ("DEPRECATED"      . "#E8A2AF")
     ("NOTE"       . "#ABE9B3"))))

(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-auto-enabled nil)
  :config
  (set-face-foreground 'highlight-indent-guides-character-face "#45475a")
  (set-face-foreground 'highlight-indent-guides-top-character-face "#cba6f7")
  (set-face-foreground 'highlight-indent-guides-stack-character-face "#45475a"))

(use-package minions
  :init
  (minions-mode 1))

(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*xref\\*"
          "^\\*vterm.*\\*$"
          "^\\*eshell.*\\*$"
          eshell-mode
          vterm-mode
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
  (setq popper-group-function #'popper-group-by-project))

(use-package beacon
  :custom
  (beacon-blink-when-window-scrolls t)
  (beacon-blink-when-window-changes t)
  (beacon-blink-when-point-moves-vertically t)
  :config
  (beacon-mode 1))

(use-package eldoc-box
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t)
  (set-face-attribute 'eldoc-box-border nil :background (catppuccin-color 'surface2))
  :general
  (:states 'normal
           :keymaps 'override
           "K" 'eldoc-box-help-at-point))

(provide 'config-ui)

;;; config-ui.el ends here
