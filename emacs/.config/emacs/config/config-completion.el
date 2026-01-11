;;; config-completion.el --- Configure completion engine -*- lexical-binding: t; -*-

;;; Commentary:
;;; Add and configure corfu completion engine and completion kind icons.

;;; Code:

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  (corfu-popupinfo-delay '(0.1 . 1.0))
  :config
  (corfu-popupinfo-mode)
  :bind
  (:map corfu-map
        ("C-j" . corfu-next)
        ("C-k" . corfu-previous)
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("C-SPC" . corfu-insert-separator)
        ("M-h" . corfu-popupinfo-toggle)
        ("M-j" . corfu-popupinfo-scroll-up)
        ("M-k" . corfu-popupinfo-scroll-down))
  :general
  (:states 'insert
           "C-SPC" 'completion-at-point))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev t)
  (add-hook 'completion-at-point-functions #'cape-file t)
  (add-hook 'completion-at-point-functions #'cape-elisp-block t)
  (add-hook 'completion-at-point-functions #'cape-history t)
  :bind
  ("C-c p" . cape-prefix-map))

(use-package emacs
  :ensure nil
  :custom
  ;; TAB cycles candidates if completion is open, otherwise indents
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil))

(provide 'config-completion)

;;; config-completion.el ends here
