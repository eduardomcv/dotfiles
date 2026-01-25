;;; config-code.el --- Language configurations and utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;; Language configurations and utilities

;;; Code:

(use-package
 emacs
 :ensure nil
 :custom
 (tab-always-indent 'complete)
 (text-mode-ispell-word-completion nil))

(use-package
 corfu
 :init (global-corfu-mode)
 :custom
 (corfu-cycle t)
 (corfu-auto t)
 (corfu-auto-delay 0.2)
 (corfu-auto-prefix 2)
 (corfu-quit-no-match 'separator)
 (corfu-popupinfo-delay '(0.1 . 1.0))
 :config (corfu-popupinfo-mode)
 :bind
 (:map
  corfu-map
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
 :general (:states 'insert "C-SPC" 'completion-at-point))

(use-package
 kind-icon
 :after corfu
 :custom
 (kind-icon-blend-background t)
 (kind-icon-default-face 'corfu-default)
 :config
 (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package
 cape
 :init
 (add-hook 'completion-at-point-functions #'cape-dabbrev t)
 (add-hook 'completion-at-point-functions #'cape-file t)
 (add-hook 'completion-at-point-functions #'cape-elisp-block t)
 (add-hook 'completion-at-point-functions #'cape-history t)
 :bind ("C-c p" . cape-prefix-map))

(use-package
 markdown-mode
 :mode
 (("README\\.md\\'" . gfm-mode) ("\\.md\\'" . markdown-mode))
 :init (setq markdown-command "pandoc")
 :custom (markdown-fontify-code-blocks-natively t))

(use-package kotlin-ts-mode)

(use-package
 treesit
 :ensure nil
 :mode
 (("\\.tsx\\'" . tsx-ts-mode)
  ("\\.ts\\'" . typescript-ts-mode)
  ("\\.yml\\'" . yaml-ts-mode)
  ("Dockerfile\\'" . dockerfile-ts-mode)
  ("\\.rs\\'" . rust-ts-mode)
  ("\\.go\\'" . go-ts-mode)
  ("go\\.mod\\'" . go-mod-ts-mode)
  ("\\.kt\\'" . kotlin-ts-mode))
 :preface
 (setq
  treesit-language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (javascript
     "https://github.com/tree-sitter/tree-sitter-javascript")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (markdown
     "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
     "split_parser"
     "tree-sitter-markdown/src")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx
     "https://github.com/tree-sitter/tree-sitter-typescript"
     "master"
     "tsx/src")
    (typescript
     "https://github.com/tree-sitter/tree-sitter-typescript"
     "master"
     "typescript/src")
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")
    (dockerfile
     "https://github.com/camdencheek/tree-sitter-dockerfile")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
    (ruby "https://github.com/tree-sitter/tree-sitter-ruby")))
 :custom (treesit-font-lock-level 4)
 (major-mode-remap-alist
  '((python-mode . python-ts-mode)
    (javascript-mode . js-ts-mode)
    (js-json-mode . json-ts-mode)
    (conf-toml-mode . toml-ts-mode)
    (bash-mode . bash-ts-mode)
    (sh-mode . bash-ts-mode)
    (css-mode . css-ts-mode)
    (json-mode . json-ts-mode)
    (html-mode . html-ts-mode)
    (ruby-mode . ruby-ts-mode)))
 :config
 (defun custom/treesit-install-grammars ()
   "Install all tree-sitter grammars defined in `treesit-language-source-alist`."
   (interactive)
   (dolist (grammar treesit-language-source-alist)
     (treesit-install-language-grammar (car grammar)))))

(use-package
 pyvenv
 :custom (pyvenv-default-virtual-env-name ".venv")
 :config (pyvenv-mode 1)

 (defun custom/auto-activate-python-venv ()
   "Activate .venv directory if it exists in the project root."
   (let* ((project-root
           (locate-dominating-file buffer-file-name ".venv"))
          (venv-path
           (and project-root
                (expand-file-name ".venv" project-root))))
     (when (and venv-path (file-directory-p venv-path))
       (pyvenv-activate venv-path))))

 (add-hook 'python-mode-hook #'custom/auto-activate-python-venv)
 (add-hook 'python-ts-mode-hook #'custom/auto-activate-python-venv))

(use-package
 yasnippet
 :init (yas-global-mode 1)
 :config (yas-reload-all))

(use-package yasnippet-snippets :after yasnippet)

(provide 'config-code)

;;; config-code.el ends here
