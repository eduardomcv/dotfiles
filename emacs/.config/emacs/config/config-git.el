;;; config-git.el --- Git workspace configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configures Magit client and diff highlights near the number column.

;;; Code:

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))

  :init
  (global-diff-hl-mode)

  :custom
  (diff-hl-margin-symbols-alist
   '((insert . "│")
     (delete . "│")
     (change . "│")
     (unknown . "│")
     (ignored . "│")))

  :config
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)

  (set-face-attribute 'diff-hl-insert nil :inherit 'diff-added :background 'unspecified)
  (set-face-attribute 'diff-hl-delete nil :inherit 'diff-removed :background 'unspecified)
  (set-face-attribute 'diff-hl-change nil :inherit 'diff-changed :background 'unspecified))

(provide 'config-git)

;;; config-git.el ends here
