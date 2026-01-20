;;; config-finder.el --- Finder configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configures search commands using consult combined with orderless search results
;;; to provide "fuzzy-finder" utilities.

;;; Code:

(use-package
 consult
 :init
 (setq xref-show-xrefs-function #'consult-xref)
 (setq xref-show-definitions-function #'consult-xref)

 :custom (consult-async-min-input 0)

 :config
 (let ((fd-name
        (if (executable-find "fdfind")
            "fdfind"
          "fd")))
   (setq
    consult-fd-args
    (concat
     fd-name
     " --full-path --absolute-path --color=never --hidden --exclude .git")))

 (setq
  consult-ripgrep-args
  "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip --hidden -g !.git")

 (consult-customize
  consult-ripgrep
  consult-git-grep
  consult-grep
  consult-bookmark
  consult-recent-file
  consult-xref
  :preview-key '(:debounce 0.2 any))

 (defun custom/consult-ripgrep-at-point ()
   "Run consult-ripgrep with the current symbol at point."
   (interactive)
   (let ((symbol (thing-at-point 'symbol t)))
     (consult-ripgrep nil (or symbol ""))))

 (defun custom/consult-ripgrep-region ()
   "Run consult-ripgrep with the current visual selection."
   (interactive)
   (let ((text
          (when (use-region-p)
            (buffer-substring-no-properties
             (region-beginning) (region-end)))))
     (deactivate-mark)
     (when text
       (consult-ripgrep nil (regexp-quote text)))))

 (defun custom/consult-ripgrep-dwim ()
   "Run grep on region if visual selection, normal grep otherwise."
   (interactive)
   (if (evil-visual-state-p)
       (custom/consult-ripgrep-region)
     (call-interactively 'consult-ripgrep)))

 :general
 (custom/leader-key
  :states
  '(normal visual)
  "sg"
  '(custom/consult-ripgrep-dwim :which-key "grep")
  :states
  'normal
  "sw"
  '(custom/consult-ripgrep-at-point :which-key "search word")
  "sb"
  '(consult-buffer :which-key "switch buffer")
  "ss"
  '(consult-line :which-key "current file")
  "bl"
  '(consult-line :which-key "search buffer lines")
  "sr"
  '(consult-recent-file :which-key "recent files")
  "pb"
  '(consult-project-buffer :which-key "project buffers")))

(use-package
 orderless
 :custom (completion-styles '(orderless basic))
 (completion-category-overrides
  '((file (styles basic partial-completion)))))

(use-package
 consult-todo
 :config
 (setq consult-todo--narrow
       '((?t . "TODO")
         (?f . "FIXME")
         (?h . "HACK")
         (?d . "DEPRECATED")
         (?n . "NOTE")))
 :general
 (custom/leader-key
  "st"
  '(consult-todo-project :which-key "project todos") "bt"
  '(consult-todo :which-key "search buffer todos")))

(use-package wgrep :custom (wgrep-auto-save-buffer t))

(use-package
 embark
 :init
 (setq prefix-help-command #'embark-prefix-help-command)
 (add-hook 'context-menu-functions #'embark-context-menu 100)
 :config
 (add-to-list
  'display-buffer-alist
  '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
    nil
    (window-parameters (mode-line-format . none))))
 :bind
 (("C-q" . embark-act)
  ("C-;" . embark-dwim)
  ("C-h B" . embark-bindings)))

(use-package
 embark-consult
 :hook (embark-collect-mode . consult-preview-at-point-mode))


(provide 'config-finder)

;;; config-finder.el ends here
