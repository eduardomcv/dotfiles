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
        :request-params
        '(:thinking
          (:type "enabled" :budget_tokens 2048)
          :max_tokens 4096)))

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

(use-package
 copilot
 :hook (prog-mode . copilot-mode)
 :custom (copilot-idle-delay 0.5)
 :config
 (add-hook 'evil-insert-state-exit-hook 'copilot-clear-overlay)
 :general
 (:states
  'insert
  :keymaps
  'copilot-mode-map
  "C-<tab>"
  #'copilot-accept-completion
  "C-M-<tab>"
  #'copilot-accept-completion-by-word
  "C-M-<right>"
  #'copilot-accept-completion-by-word))

(provide 'config-ai)

;;; config-ai.el ends here
