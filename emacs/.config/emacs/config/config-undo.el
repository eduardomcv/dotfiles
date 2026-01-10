;;; config-undo.el --- Undo configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure tree-like behavior for undo.

;;; Code:

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history t)
  ;; Store undo files in var/undo-tree-hist/
  (undo-tree-history-directory-alist
   `(("." . ,(no-littering-expand-var-file-name "undo-tree-hist/")))))

(provide 'config-undo)

;;; config-undo.el ends here
