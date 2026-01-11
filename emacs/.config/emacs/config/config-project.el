;;; config-project.el --- Project detection -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure project search and detection.

;;; Code:

(use-package project
  :ensure nil
  :config
  (setq project-switch-commands
        '((project-find-file "Find file" ?f)
          (project-dired "Dired" ?d)
          (consult-project-buffer "Buffer" ?b)
          (consult-ripgrep "Ripgrep" ?g)
          (magit-project-status "Magit" ?G)))
  :general
  (:states 'normal
           "C-p" 'project-find-file)
  (custom/leader-keys
    "sf" '(project-find-file :which-key "project files")
    "pp" '(project-switch-project :which-key "switch project")))

(provide 'config-project)

;;; config-project.el ends here
