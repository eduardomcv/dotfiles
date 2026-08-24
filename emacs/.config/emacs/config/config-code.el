;;; config-code.el --- Code configurations -*- lexical-binding: t; -*-

;;; Commentary:
;;; Language configurations, linters, code formatting and other code utilities.

;;; Code:

(use-package
 flycheck
 :init (global-flycheck-mode)
 :general
 (:states
  'normal "[ d" 'flycheck-previous-error "] d" 'flycheck-next-error))

(use-package
 flycheck-inline
 :after flycheck
 :hook (flycheck-mode . flycheck-inline-mode)
 :general
 (custom/leader-key
  :states
  'normal
  "ce"
  '(flycheck-inline-mode :which-key "toggle inline errors")))

(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode))

(use-package
 apheleia
 :init (apheleia-global-mode 1)
 :custom (apheleia-formatters-respect-indent-level nil)
 :config
 ;; Replace the default (black) with ruff. emacs-lisp-mode is absent:
 ;; `elisp-autofmt-mode' above already formats it on save.
 (setf (alist-get 'python-mode apheleia-mode-alist)
       '(ruff-isort ruff))
 (setf (alist-get 'python-ts-mode apheleia-mode-alist)
       '(ruff-isort ruff))

 :general
 (custom/leader-key
  "cf" '(apheleia-format-buffer :which-key "format buffer")))

(use-package
 treesit
 :ensure nil
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
    (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
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
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (ruby "https://github.com/tree-sitter/tree-sitter-ruby")))
 :custom (treesit-font-lock-level 4)
 ;; `html-mode'/`yaml-mode' absent: they get an explicit `auto-mode-alist'
 ;; entry below instead, since `.html'/`.yaml' don't map to them anyway.
 (major-mode-remap-alist
  '((python-mode . python-ts-mode)
    (javascript-mode . js-ts-mode)
    (js-json-mode . json-ts-mode)
    (conf-toml-mode . toml-ts-mode)
    (bash-mode . bash-ts-mode)
    (sh-mode . bash-ts-mode)
    (css-mode . css-ts-mode)
    (json-mode . json-ts-mode)
    (ruby-mode . ruby-ts-mode)
    (lua-mode . lua-ts-mode)))
 :config
 (defun custom/treesit-register-auto-modes ()
   "Map filenames to a `-ts-mode' once its grammar is installed.
Safe to call repeatedly; `config-bootstrap.el' re-runs this once a
missing grammar finishes compiling."
   (dolist
       (entry
        '(("\\.tsx\\'" tsx . tsx-ts-mode)
          ("\\.ts\\'" typescript . typescript-ts-mode)
          ("\\.mjs\\'" javascript . js-ts-mode)
          ("\\.cjs\\'" javascript . js-ts-mode)
          ("Dockerfile\\'" dockerfile . dockerfile-ts-mode)
          ("\\.rs\\'" rust . rust-ts-mode)
          ("\\.lua\\'" lua . lua-ts-mode)
          ("\\.ya?ml\\'" yaml . yaml-ts-mode)))
     (when (treesit-language-available-p (cadr entry))
       (add-to-list 'auto-mode-alist (cons (car entry) (cddr entry)))))

   ;; Flutter/Dart localization files are plain JSON.
   (when (treesit-language-available-p 'json)
     (add-to-list 'auto-mode-alist '("\\.arb\\'" . json-ts-mode))))

 (custom/treesit-register-auto-modes))

(use-package
 lsp-mode
 :init
 (defun custom/add-orderless-to-lsp-mode-completion ()
   (setf (alist-get
          'styles
          (alist-get 'lsp-capf completion-category-defaults))
         '(orderless)))
 (setq lsp-keymap-prefix "C-c l")
 :hook
 ((lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . custom/add-orderless-to-lsp-mode-completion)
  (js-ts-mode . lsp-deferred)
  (typescript-ts-mode . lsp-deferred)
  (tsx-ts-mode . lsp-deferred)
  (yaml-ts-mode . lsp-deferred)
  (json-ts-mode . lsp-deferred)
  (css-ts-mode . lsp-deferred)
  ;; `.html' resolves to `mhtml-mode', not `html-ts-mode'; see above.
  (mhtml-mode . lsp-deferred)
  (ruby-ts-mode . lsp-deferred)
  (lua-ts-mode . lsp-deferred)
  (bash-ts-mode . lsp-deferred)
  (rust-ts-mode . lsp-deferred)
  (python-ts-mode . lsp-deferred))
 :commands (lsp lsp-deferred)
 :custom
 (lsp-completion-provider :none)
 (lsp-diagnostics-provider :flycheck)
 (lsp-idle-delay 0.500)
 (lsp-modeline-diagnostics-enable nil)
 (lsp-modeline-code-action-fallback-icon " ")

 ;; Both attach to the same buffer; lsp-mode (unlike eglot) allows it.
 (lsp-ruff-server-command '("ruff" "server"))
 (lsp-python-ty-clients-server-command '("ty" "server"))

 (lsp-javascript-update-imports-on-file-move-enabled "always")
 (lsp-javascript-suggest-complete-function-calls t)
 (lsp-javascript-implicit-project-config-check-js t)

 (lsp-typescript-update-imports-on-file-move-enabled "always")
 (lsp-typescript-suggest-complete-function-calls t)

 (lsp-eslint-server-command
  '("vscode-eslint-language-server" "--stdio"))
 (lsp-eslint-run "onSave")
 (lsp-eslint-auto-fix-on-save t)
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
 (lsp-ui-doc-show-with-cursor nil)
 (lsp-ui-doc-show-with-mouse nil)
 (lsp-ui-sideline-enable nil)
 (lsp-ui-sideline-show-diagnostics nil)
 (lsp-headerline-breadcrumb-enable-diagnostics nil)
 :config
 (setq lsp-ui-doc-border (catppuccin-color 'surface2))
 (set-face-attribute 'lsp-ui-doc-background nil
                     :background (catppuccin-color 'base))
 :general
 (:states
  'normal
  :keymaps
  'lsp-mode-map
  "K"
  #'lsp-ui-doc-glance
  "M-j"
  #'lsp-ui-doc-scroll-up
  "M-k"
  #'lsp-ui-doc-scroll-down))

(use-package
 mason
 :ensure nil
 :general (custom/leader-key "cm" '(mason-manager :which-key "mason")))

(use-package
 dap-mode
 :defer t
 :custom (dap-python-debugger 'debugpy)
 :config
 ;; `dap-js' (not the older `dap-node') registers the "pwa-node" adapter.
 ;; Point both it and dap-python at mason.el's own installs.
 (let ((mason-python
        (expand-file-name "packages/debugpy/bin/python" mason-dir))
       (mason-js-debug
        (expand-file-name
         "packages/js-debug-adapter/js-debug/src/dapDebugServer.js"
         mason-dir)))
   (when (file-exists-p mason-python)
     (setq dap-python-executable mason-python))
   (when (file-exists-p mason-js-debug)
     (setq dap-js-debug-program (list "node" mason-js-debug))))
 (require 'dap-js)
 (require 'dap-python)
 :general
 (custom/leader-key
  "d"
  '(:ignore t :which-key "debug")
  "dc"
  '(dap-continue :which-key "continue")
  "ds"
  '(dap-next :which-key "step over")
  "di"
  '(dap-step-in :which-key "step into")
  "do"
  '(dap-step-out :which-key "step out")
  "dr"
  '(dap-debug-restart :which-key "restart")
  "dt"
  '(dap-delete-session :which-key "terminate")
  "dd"
  '(dap-disconnect :which-key "disconnect")
  "db"
  '(dap-breakpoint-toggle :which-key "toggle breakpoint")
  "dB"
  '(dap-breakpoint-condition :which-key "conditional breakpoint")
  "dl"
  '(dap-breakpoint-log-message :which-key "log point")
  "du"
  '(dap-ui-many-windows-mode :which-key "toggle ui")
  "dR"
  '(dap-ui-repl :which-key "toggle repl")
  :states
  'normal
  "dh"
  '(dap-eval-thing-at-point :which-key "evaluate")
  :states
  'visual
  "dh"
  '(dap-eval-region :which-key "evaluate selection")))

(use-package dart-mode :mode (("\\.dart\\'" . dart-mode)))
(use-package
 lsp-dart
 :hook (dart-mode . lsp-deferred)
 :general
 (custom/leader-key
  :keymaps
  'dart-mode-map
  "ut"
  '(lsp-dart-run-test-at-point :which-key "test nearest")
  "uf"
  '(lsp-dart-run-test-file :which-key "test file")))

(use-package
 jest-test-mode
 :hook
 ((js-ts-mode typescript-ts-mode tsx-ts-mode) . jest-test-mode)
 :general
 (custom/leader-key "u" '(:ignore t :which-key "test"))
 (custom/leader-key
  :keymaps
  'jest-test-mode-map
  "ut"
  '(jest-test-run-at-point :which-key "test nearest")
  "uf"
  '(jest-test-run :which-key "test file")))

(use-package dotenv-mode :mode (("\\.env\\..*\\'" . dotenv-mode)))

(use-package
 markdown-mode
 :mode
 (("README\\.md\\'" . gfm-mode) ("\\.md\\'" . markdown-mode))
 ;; pandoc isn't in the Mason registry; fall back to the built-in renderer.
 :init
 (when (executable-find "pandoc")
   (setq markdown-command "pandoc"))
 :custom (markdown-fontify-code-blocks-natively t))

(use-package
 pyvenv
 :custom (pyvenv-default-virtual-env-name ".venv")
 :config (pyvenv-mode 1)

 (defconst custom/python-venv-names '(".venv" "venv" ".env" "env")
   "Directory names checked, in priority order, for a Python virtualenv.")

 (defun custom/python-venv-p (dir)
   "Return non-nil if DIR is an actual virtualenv, not just a same-named dir."
   (or (file-exists-p (expand-file-name "pyvenv.cfg" dir))
       (file-executable-p (expand-file-name "bin/python" dir))))

 ;; Captured once at startup, before `pyvenv-activate' can set this same
 ;; variable itself and be mistaken for an external one on a later read.
 (defconst custom/python-external-venv
   (let ((env (getenv "VIRTUAL_ENV")))
     (and env (file-directory-p env) (file-name-as-directory env)))
   "VIRTUAL_ENV as exported before Emacs started (direnv, tox, ...), if any.")

 (defun custom/python-venv-root (&optional file)
   "Find the Python virtualenv covering FILE (default: current buffer)."
   (or custom/python-external-venv
       (let ((file (or file buffer-file-name)))
         (and file
              (seq-some
               (lambda (name)
                 (when-let ((root (locate-dominating-file file name)))
                   (let ((candidate (expand-file-name name root)))
                     (and (custom/python-venv-p candidate) candidate))))
               custom/python-venv-names)))))

 (defun custom/python-sync-venv ()
   "Activate this buffer's virtualenv, or deactivate if it has none.
`pyvenv-activate' mutates PATH globally, so without the deactivate
branch a previous project's venv stays stuck active."
   (if-let ((venv (custom/python-venv-root)))
       (unless (equal (file-name-as-directory venv) pyvenv-virtual-env)
         (pyvenv-activate venv))
     (when pyvenv-virtual-env
       (pyvenv-deactivate))))

 (add-hook 'python-ts-mode-hook #'custom/python-sync-venv))

(use-package
 python-pytest
 :general
 (custom/leader-key
  :keymaps
  '(python-mode-map python-ts-mode-map)
  "ut"
  '(python-pytest-run-def-at-point-treesit :which-key "test nearest")
  "uf"
  '(python-pytest-file-dwim :which-key "test file")))

(use-package
 yasnippet
 :init (yas-global-mode 1)
 :config (yas-reload-all))

(use-package yasnippet-snippets :after yasnippet)

(provide 'config-code)

;;; config-code.el ends here
