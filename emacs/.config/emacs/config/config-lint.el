;;; config-lint.el --- Linting -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure flymake for linting code.

;;; Code:

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :config
  (define-fringe-bitmap 'flymake-fringe-bitmap-circle
    (vector #b00000000
            #b00111100
            #b01111110
            #b01111110
            #b01111110
            #b01111110
            #b00111100
            #b00000000))

  (setq flymake-error-bitmap '(flymake-fringe-bitmap-circle compilation-error))
  (setq flymake-warning-bitmap '(flymake-fringe-bitmap-circle compilation-warning))
  (setq flymake-note-bitmap '(flymake-fringe-bitmap-circle compilation-info))

  (set-face-attribute 'flymake-error nil :underline '(:style wave :color "#F28FAD"))
  (set-face-attribute 'flymake-warning nil :underline '(:style wave :color "#FAE3B0"))
  (set-face-attribute 'flymake-note nil :underline '(:style wave :color "#96CDFB"))

  :general
  (:states 'normal
           "[d" 'flymake-goto-prev-error
           "]d" 'flymake-goto-next-error))

(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup))

(use-package flymake-eslint
  :hook ((javascript-mode . flymake-eslint-enable)
         (js-ts-mode . flymake-eslint-enable)
         (typescript-ts-mode . flymake-eslint-enable)
         (tsx-ts-mode . flymake-eslint-enable)))

(provide 'config-lint)

;;; config-lint.el ends here
