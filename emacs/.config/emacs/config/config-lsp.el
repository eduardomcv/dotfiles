;;; config-lsp.el --- LSP configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configurations for Eglot and interaction with LSP servers.

;;; Code:

(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :config
  (when (fboundp 'cape-wrap-buster)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

  (put 'tsx-ts-mode 'eglot-language-id "typescriptreact")

  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode typescript-mode javascript-mode js-jsx-mode) . ("vtsls" "--stdio")))

  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("basedpyright-langserver" "--stdio")))

  (setq-default eglot-workspace-configuration
                '(:basedpyright (:analysis (:autoSearchPaths t
                                                             :useLibraryCodeForTypes t
                                                             :diagnosticMode "openFilesOnly"))

                                :vtsls (:enableMoveToFileCodeAction t
                                                                    :experimental (:completion (:enableServerSideFuzzyMatch t)))

                                :typescript (:maxInlayHintLength 30
                                                                 :updateImportsOnFileMove (:enabled "always")
                                                                 :suggest (:completeFunctionCalls t)
                                                                 :preferences (:includePackageJsonAutoImports "on"
                                                                                                              :importModuleSpecifier "non-relative")
                                                                 :inlayHints (:parameterNames (:enabled "all")
                                                                                              :variableTypes (:enabled t)))

                                :javascript (:maxInlayHintLength 30
                                                                 :updateImportsOnFileMove (:enabled "always")
                                                                 :suggest (:completeFunctionCalls t)
                                                                 :inlayHints (:parameterNames (:enabled "all")
                                                                                              :variableTypes (:enabled t)))))

  :general
  (:states 'normal
           "gD" 'eglot-find-declaration
           "gI" 'eglot-find-implementation)
  (custom/leader-keys
    "ca" '(eglot-code-actions :which-key "actions")
    "cr" '(eglot-rename :which-key "rename")
    "ci" '(eglot-inlay-hints-mode :which-key "inlay hints")))

(provide 'config-lsp)

;;; config-lsp.el ends here
