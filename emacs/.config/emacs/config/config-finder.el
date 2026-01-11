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

  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip --hidden -g !.git")

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key '(:debounce 0.2 any))

  :general
  (custom/leader-keys
    "sg" '(consult-ripgrep :which-key "grep")
    "sb" '(consult-buffer :which-key "switch buffer")
    "ss" '(consult-line :which-key "current file")
    "bl" '(consult-line :which-key "search buffer lines")
    "sr" '(consult-recent-file :which-key "recent files")
    "pb" '(consult-project-buffer :which-key "project buffers")))

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
          (?n . "NOTE")))
  :general
  (custom/leader-keys
    "st" '(consult-todo-project :which-key "project todos")
    "bt" '(consult-todo :which-key "search buffer todos")))

(provide 'config-finder)

;;; config-finder.el ends here
