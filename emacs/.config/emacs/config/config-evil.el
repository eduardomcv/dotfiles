;;; config-evil.el --- evil configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Evil provides evil-mode for vi emulation.

;;; Code:

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)

  (setq evil-undo-system 'undo-tree)

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

(provide 'config-evil)

;;; config-evil.el ends here
