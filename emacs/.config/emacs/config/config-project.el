;;; config-project.el --- Project detection -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure project search and detection.

;;; Code:

(use-package project
  :ensure nil
	:config
  (with-eval-after-load 'consult
    (setq project-switch-commands 
          '((project-find-file "Find file")
            (project-dired "Dired")
            (consult-project-buffer "Buffer")
            (consult-ripgrep "Ripgrep")
            (magit-project-status "Magit")))))

(provide 'config-project)

;;; config-project.el ends here
