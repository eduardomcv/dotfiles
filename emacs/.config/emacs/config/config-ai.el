;;; config-ai.el --- AI tool integration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configuration for the gptel LLM client and Copilot suggestions.

;;; Code:

(use-package
 gptel
 :config (setq gptel-model 'claude-sonnet-4-6)

 (setq gptel-backend
       (gptel-make-anthropic
        "Claude-thinking"
        :key gptel-api-key
        :stream t
        :models '(claude-sonnet-4-6)
        :request-params '(:thinking (:type "enabled" :budget_tokens 2048) :max_tokens 4096)))

 :general
 (custom/leader-key
  "a"
  '(:ignore t :which-key "AI")
  "a c"
  '(gptel :which-key "open chat")
  "a s"
  '(gptel-send :which-key "send to gptel")
  "a m"
  '(gptel-menu :which-key "gptel menu")))

;; `lsp-copilot'/`lsp-inline-completion' are separate files from core
;; lsp-mode and must be required explicitly before their variables exist.
(use-package
 lsp-mode
 :ensure nil
 :hook (lsp-mode . lsp-inline-completion-mode)
 :config (require 'lsp-copilot) (require 'lsp-inline-completion)
 ;; copilot-language-server has notifications that lsp-copilot.el's
 ;; handler table doesn't know about; each one pops *Warnings*.
 ;; These are all cosmetic/telemetry.
 (when-let* ((client (gethash 'copilot-ls lsp-clients))
             (handlers (lsp--client-notification-handlers client)))
   (dolist (method
            '("copilot/hook/didChange"
              "copilot/customAgent/didChange"
              "copilot/quotaChange"
              "copilot/quotaWarning"
              "didChangeStatus/v2"
              "policy/didChange"))
     (unless (gethash method handlers)
       (puthash method #'ignore handlers))))
 (setq lsp-copilot-enabled t) (setq lsp-inline-completion-idle-delay 0.5)
 ;; Accept with the same key `copilot.el' used to use.
 (define-key lsp-inline-completion-active-map (kbd "C-<tab>") #'lsp-inline-completion-accept)
 :general (:states 'insert :keymaps 'lsp-mode-map "C-<tab>" #'lsp-inline-completion-display))

(provide 'config-ai)

;;; config-ai.el ends here
