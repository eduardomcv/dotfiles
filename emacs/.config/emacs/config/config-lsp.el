;;; config-lsp.el --- LSP configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configurations for Eglot and interaction with LSP servers.

;;; Code:

(use-package eglot
  :hook ((python-mode . eglot-ensure)
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
         (html-ts-mode . eglot-ensure))
  :init
  (defvar custom/eglot-inlay-hints-enabled nil
    "Global toggle state for Eglot inlay hints.")
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect nil)
  :config
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (ignore-errors
                (if custom/eglot-inlay-hints-enabled
                    (eglot-inlay-hints-mode 1)
                  (eglot-inlay-hints-mode -1)))))

  (defun custom/toggle-inlay-hints()
    "Toggle Eglot inlay hints."
    (interactive)
    (setq custom/eglot-inlay-hints-enabled (not custom/eglot-inlay-hints-enabled))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (bound-and-true-p eglot--managed-mode)
                   (fboundp 'eglot-inlay-hints-mode))
          (ignore-errors
            (eglot-inlay-hints-mode (if custom/eglot-inlay-hints-enabled 1 -1))))))
    (message "Global Inlay Hints: %s"
             (if custom/eglot-inlay-hints-enabled "ON" "OFF")))

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
    "ci" '(custom/toggle-inlay-hints :which-key "toggle inlay hints")))

(provide 'config-lsp)

;;; config-lsp.el ends here
