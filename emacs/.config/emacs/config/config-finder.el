;;; config-finder.el --- Finder configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configures search commands using consult combined with orderless search results
;;; to provide "fuzzy-finder" utilities.

;;; Code:
(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  :custom
  (consult-async-min-input 0)

  :config
  (let ((fd-name (if (executable-find "fdfind") "fdfind" "fd")))
    (setq consult-fd-args (concat fd-name " --full-path --absolute-path --color=never --hidden --exclude .git")))

  :bind (
         ("C-c g" . consult-ripgrep)
         ("C-c f" . consult-fd)
         ("C-c b" . consult-buffer)
         ("C-c s" . consult-line)
         ))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult-todo
  :config
  (setq consult-todo--narrow
    '((?t . "TODO")
      (?f . "FIXME")
      (?h . "HACK")
      (?d . "DEPRECATED")
      (?n . "NOTE"))))

(provide 'config-finder)

;;; config-finder.el ends here
