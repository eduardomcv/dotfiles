;;; config-code.el --- Language configurations and utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;; Language configurations and utilities (LSP, tree-sitter).

;;; Code:

(use-package
 flycheck
 :init (global-flycheck-mode)
 :general
 (:states
  'normal "[d" 'flycheck-previous-error "]d" 'flycheck-next-error))

(use-package
 lsp-mode
 :init
 (setq lsp-keymap-prefix "C-c l")
 (lsp-dired-mode)
 :hook
 ((javascript-mode . lsp-deferred)
  (js-ts-mode . lsp-deferred)
  (typescript-ts-mode . lsp-deferred)
  (tsx-ts-mode . lsp-deferred)
  (yaml-ts-mode . lsp-deferred)
  (js-json-mode . lsp-deferred)
  (json-ts-mode . lsp-deferred)
  (css-mode . lsp-deferred)
  (css-ts-mode . lsp-deferred)
  (html-mode . lsp-deferred)
  (html-ts-mode . lsp-deferred)
  (ruby-mode . lsp-deferred)
  (ruby-ts-mode . lsp-deferred)
  (kotlin-ts-mode . lsp-deferred)
  (go-ts-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration))
 :commands (lsp lsp-deferred)
 :custom
 (lsp-completion-provider :none)
 (lsp-diagnostics-provider :flycheck)
 (lsp-idle-delay 0.500)
 (lsp-modeline-diagnostics-enable nil)
 (lsp-modeline-code-action-fallback-icon " ")

 (lsp-go-gopls-opts '((matcher . "CaseSensitive") (staticcheck . t)))

 (lsp-javascript-update-imports-on-file-move-enabled "always")
 (lsp-javascript-suggest-complete-function-calls t)
 (lsp-javascript-implicit-project-config-check-js t)

 (lsp-typescript-update-imports-on-file-move-enabled "always")
 (lsp-typescript-suggest-complete-function-calls t)

 (lsp-eslint-server-command
  '("vscode-eslint-language-server" "--stdio"))
 :config
 (lsp-register-custom-settings
  '(("javascript.preferences.quoteStyle" "auto")
    ("typescript.preferences.quoteStyle" "auto")
    ("typescript.preferences.includePackageJsonAutoImports" "on")
    ("typescript.preferences.importModuleSpecifier" "non-relative")))
 :general
 (:states
  'normal
  :keymaps
  'lsp-mode-map
  "gD"
  'lsp-find-declaration
  "gd"
  'lsp-find-definition
  "gI"
  'lsp-find-implementation
  "gr"
  'lsp-find-references)
 (custom/leader-key
  :states 'normal
  :keymaps
  'lsp-mode-map
  "ca"
  '(lsp-execute-code-action :which-key "actions")
  "cr"
  '(lsp-rename :which-key "rename")))

(use-package
 lsp-ui
 :commands lsp-ui-mode
 :custom
 (lsp-ui-doc-position 'at-point)
 (lsp-ui-doc-border (catppuccin-color 'surface2))
 (lsp-ui-doc-show-with-cursor nil)
 (lsp-ui-doc-show-with-mouse nil)
 :config
 (set-face-attribute 'lsp-ui-doc-background nil
                     :background (catppuccin-color 'base))
 :general
 (:states
  'normal
  :keymaps
  'lsp-mode-map
  "K"
  'lsp-ui-doc-glance
  "M-j"
  'lsp-ui-doc-scroll-up
  "M-k"
  'lsp-ui-doc-scroll-down))

(use-package
 lsp-pyright
 :custom
 (lsp-pyright-langserver-command "basedpyright")
 (lsp-pyright-auto-search-paths t)
 (lsp-pyright-use-library-code-for-types t)
 (lsp-pyright-diagnostic-mode "openFilesOnly")
 :hook
 ((python-mode
   .
   (lambda ()
     (require 'lsp-pyright)
     (lsp-deferred)))
  (python-ts-mode
   .
   (lambda ()
     (require 'lsp-pyright)
     (lsp-deferred)))))


(use-package dap-mode)

(use-package dart-mode :mode (("\\.dart\\'" . dart-mode)))
(use-package lsp-dart :hook (dart-mode . lsp-deferred))

(use-package yaml-mode)
(use-package dotenv-mode :mode (("\\.env\\..*\\'" . dotenv-mode)))

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
  ("Dockerfile\\'" . dockerfile-ts-mode)
  ("\\.rs\\'" . rust-ts-mode)
  ("\\.go\\'" . go-ts-mode)
  ("go\\.mod\\'" . go-mod-ts-mode)
  ("\\.kt\\'" . kotlin-ts-mode)
  ("\\.kts\\'" . kotlin-ts-mode))
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
    (ruby-mode . ruby-ts-mode)
    (yaml-mode . yaml-ts-mode)))
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
