;;; config-code.el --- Language configurations and utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;; Language configurations and utilities (LSP, tree-sitter).

;;; Code:

(use-package
 emacs
 :ensure nil
 :custom
 (tab-always-indent 'complete)
 (text-mode-ispell-word-completion nil))

(use-package
 eglot
 :ensure nil

 :hook
 ((python-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (javascript-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (yaml-ts-mode . eglot-ensure)
  (js-json-mode . eglot-ensure)
  (json-ts-mode . eglot-ensure)
  (css-mode . eglot-ensure)
  (css-ts-mode . eglot-ensure)
  (html-mode . eglot-ensure)
  (html-ts-mode . eglot-ensure)
  (ruby-mode . eglot-ensure)
  (ruby-ts-mode . eglot-ensure)
  (kotlin-ts-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  (dart-mode . eglot-ensure))
 :init
 (defvar custom/eglot-inlay-hints-enabled nil
   "Global toggle state for Eglot inlay hints.")
 :custom
 (eglot-autoshutdown t)
 (eglot-sync-connect nil)
 (eglot-code-action-indicator " 󱠀 ")
 :config
 (add-hook
  'eglot-managed-mode-hook
  (lambda ()
    (ignore-errors
      (if custom/eglot-inlay-hints-enabled
          (eglot-inlay-hints-mode 1)
        (eglot-inlay-hints-mode -1)))))

 (defun custom/toggle-inlay-hints ()
   "Toggle Eglot inlay hints."
   (interactive)
   (setq custom/eglot-inlay-hints-enabled
         (not custom/eglot-inlay-hints-enabled))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (when (and (bound-and-true-p eglot--managed-mode)
                  (fboundp 'eglot-inlay-hints-mode))
         (ignore-errors
           (eglot-inlay-hints-mode
            (if custom/eglot-inlay-hints-enabled
                1
              -1))))))
   (message "Global Inlay Hints: %s"
            (if custom/eglot-inlay-hints-enabled
                "ON"
              "OFF")))

 (when (fboundp 'cape-wrap-buster)
   (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

 (add-to-list
  'eglot-server-programs
  `(((js-mode :language-id "javascript")
     (js-ts-mode :language-id "javascript")
     (tsx-ts-mode :language-id "typescriptreact")
     (typescript-ts-mode :language-id "typescript")
     (typescript-mode :language-id "typescript"))
    .
    ,(eglot-alternatives
      '(("rass ts")
        ("typescript-language-server" "--stdio")
        ("vtsls" "--stdio")))))

 (add-to-list
  'eglot-server-programs
  `((kotlin-mode kotlin-ts-mode)
    .
    ,(eglot-alternatives
      '(("kotlin-language-server") ("kotlin-lsp" "--stdio")))))

 (defun eglot-format-buffer-before-save ()
   (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

 (add-hook 'go-mode-hook #'eglot-format-buffer-before-save)

 (add-hook 'before-save-hook
           (lambda ()
             (call-interactively 'eglot-code-action-organize-imports))
           nil t)

 (setq-default eglot-workspace-configuration
               '(:gopls
                 (:staticcheck t :matcher "CaseSensitive")

                 :basedpyright
                 (:analysis
                  (:autoSearchPaths
                   t
                   :useLibraryCodeForTypes t
                   :diagnosticMode "openFilesOnly"))

                 :vtsls
                 (:autoUseWorkspaceTsdk
                  t
                  :enableMoveToFileCodeAction t
                  :experimental
                  (:completion (:enableServerSideFuzzyMatch t)))

                 :typescript
                 (:maxInlayHintLength
                  30
                  :updateImportsOnFileMove (:enabled "always")
                  :suggest (:completeFunctionCalls t)
                  :preferences
                  (:includePackageJsonAutoImports
                   "on"
                   :importModuleSpecifier "non-relative")
                  :inlayHints
                  (:parameterNames
                   (:enabled "all")
                   :variableTypes (:enabled t)))

                 :javascript
                 (:maxInlayHintLength
                  30
                  :implicitProjectConfig (:checkJs t)
                  :updateImportsOnFileMove (:enabled "always")
                  :suggest (:completeFunctionCalls t)
                  :inlayHints
                  (:parameterNames
                   (:enabled "all")
                   :variableTypes (:enabled t)))))
 :general
 (:states
  'normal
  "gD"
  'eglot-find-declaration
  "gI"
  'eglot-find-implementation)
 (custom/leader-key
  "ca"
  '(eglot-code-actions :which-key "actions")
  "cr"
  '(eglot-rename :which-key "rename")
  "ci"
  '(custom/toggle-inlay-hints :which-key "toggle inlay hints")))

(use-package dart-mode :mode (("\\.dart\\'" . dart-mode)))

(use-package yaml-mode)

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
