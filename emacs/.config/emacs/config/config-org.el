;;; config-org.el --- Org mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configurations for org mode, including table of contents and modern look.

;;; Code:

(use-package
 org
 :ensure nil
 :custom
 (org-directory "~/Repos/Org")
 (org-agenda-files '("~/Repos/Org/agenda.org"))
 (org-edit-src-content-indentation 0)
 (org-src-tab-acts-natively t)
 (org-src-fontify-natively t)
 (org-src-preserve-indentation t)
 (org-ellipsis "…")
 (org-auto-align-tags nil)
 (org-tags-column 0)
 (org-fold-catch-invisible-edits 'show-and-error)
 (org-special-ctrl-a/e t)
 (org-hide-emphasis-markers t)
 (org-pretty-entities t)
 (org-ellipsis "…"))

(use-package toc-org :hook (org-mode . toc-org-enable))

(use-package
 org-modern
 :hook
 ((org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)))

(provide 'config-org)

;;; config-org.el ends here
