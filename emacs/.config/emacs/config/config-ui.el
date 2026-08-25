;;; config-ui.el --- UI configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Adds UI related packages and configurations.

;;; Code:

(use-package
 emacs
 :ensure nil
 :init
 (pixel-scroll-precision-mode 1)
 (global-hl-line-mode 1)
 (global-display-line-numbers-mode 1)
 :hook ((text-mode . visual-line-mode) (org-mode . visual-line-mode))
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

 (defun custom/set-font-faces ()
   ;; Guarded: a machine without Iosevka yet would abort this :config block.
   (when (find-font (font-spec :family "Iosevka"))
     (set-face-attribute 'default nil :font "Iosevka" :height 160 :weight 'regular))
   (when (and (display-graphic-p) (require 'nerd-icons nil t))
     (nerd-icons-set-font)))

 (if (daemonp)
     (add-hook
      'after-make-frame-functions
      (lambda (frame)
        (with-selected-frame frame
          (custom/set-font-faces))))
   (custom/set-font-faces))

 ;; `load-theme' resets all faces, so re-apply these on every switch.
 (defun custom/set-font-lock-italics (&rest _)
   (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
   (set-face-attribute 'font-lock-keyword-face nil :slant 'italic))
 (add-hook 'enable-theme-functions #'custom/set-font-lock-italics)

 ;; TTY only: Symbols Nerd Font Mono is single-width, so this would
 ;; misalign icons in a GUI frame.
 (unless (display-graphic-p)
   (set-char-table-range char-width-table '(#xe000 . #xf8ff) 2)))

(use-package nerd-icons)

(use-package
 ligature
 :init (global-ligature-mode t)
 :config
 (ligature-set-ligatures
  '(prog-mode text-mode)
  '("<---"
    "<--"
    "<<-"
    "<-"
    "->"
    "-->"
    "--->"
    "<->"
    "<-->"
    "<--->"
    "<---->"
    "<!--"
    "<=="
    "<==="
    "<="
    "=>"
    "=>>"
    "==>"
    "===>"
    ">="
    "<=>"
    "<==>"
    "<===>"
    "<====>"
    "<!---"
    "<~~"
    "<~"
    "~>"
    "~~>"
    "::"
    ":::"
    "=="
    "!="
    "==="
    "!=="
    ":="
    ":-"
    ":+"
    "<*"
    "<*>"
    "*>"
    "<|"
    "<|>"
    "|>"
    "+:"
    "-:"
    "=:"
    "<******>"
    "++"
    "+++")))

(use-package
 whitespace
 :ensure nil
 :hook (prog-mode . delete-trailing-whitespace-mode)
 :custom
 (whitespace-style '(face tabs spaces trailing space-before-tab newline empty indentation missing-newline-at-eof))
 :general (custom/leader-key "cw" '(whitespace-mode :which-key "toggle whitespace")))

(use-package
 catppuccin-theme
 :custom (catppuccin-flavor 'mocha)
 :config (load-theme 'catppuccin t)
 (unless (display-graphic-p)
   (set-face-background 'default "unspecified")
   (set-face-background 'line-number "unspecified")))

(use-package eldoc :ensure nil :custom (eldoc-echo-area-use-multiline-p nil))

(use-package
 vertico
 :init (vertico-mode)
 :bind (:map vertico-map ("C-j" . vertico-next) ("C-k" . vertico-previous))
 :custom
 (vertico-resize t)
 (vertico-count 15)
 (vertico-cycle t))

(use-package marginalia :init (marginalia-mode))

(use-package
 dashboard
 :init (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
 :custom
 (dashboard-center-content t)
 (dashboard-vertically-center-content t)
 (dashboard-icon-type 'nerd-icons)
 (dashboard-display-icons-p t)
 (dashboard-set-heading-icons t)
 (dashboard-set-file-icons t)

 (dashboard-items '((recents . 5) (bookmarks . 5) (projects . 5)))

 (dashboard-item-shortcuts '((recents . "r") (bookmarks . "m") (projects . "p")))
 :config
 (set-face-attribute 'dashboard-text-banner nil :slant 'normal)
 (setq dashboard-startup-banner (expand-file-name "banner.txt" user-emacs-directory))

 (dashboard-setup-startup-hook))

(use-package
 doom-modeline
 :init (doom-modeline-mode 1)
 :custom (doom-modeline-modal-icon nil)
 :config
 (dolist (face
          '(doom-modeline-evil-normal-state
            doom-modeline-evil-insert-state
            doom-modeline-evil-visual-state
            doom-modeline-evil-operator-state
            doom-modeline-evil-replace-state
            doom-modeline-evil-motion-state))
   (set-face-attribute face nil :inverse-video t :weight 'bold)))

(use-package
 hl-todo
 :hook ((prog-mode . hl-todo-mode) (text-mode . hl-todo-mode))
 :custom (hl-todo-highlight-punctuation ":")
 (hl-todo-keyword-faces
  '(("TODO" . "#96CDFB") ("FIXME" . "#F28FAD") ("HACK" . "#FAE3B0") ("DEPRECATED" . "#E8A2AF") ("NOTE" . "#ABE9B3"))))

(use-package
 indent-bars
 :hook (prog-mode . indent-bars-mode)
 :custom
 (indent-bars-treesit-support t)
 (indent-bars-color '(highlight :face-bg t :blend 0.15))
 (indent-bars-highlight-current-depth '(:blend 0.4)))

(use-package
 colorful-mode
 :custom
 (colorful-use-prefix t)
 (colorful-only-strings 'only-prog)
 (css-fontify-colors nil)
 (mhtml-ts-mode-css-fontify-colors nil)
 :config
 (global-colorful-mode t)
 (add-to-list 'global-colorful-modes 'helpful-mode))

(provide 'config-ui)

;;; config-ui.el ends here
