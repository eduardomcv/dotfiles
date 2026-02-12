;;; config-diagnostics.el --- Diagnostics configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure flymake for code diagnostics.
;;; Integrate flymake with linters.

;;; Code:

(use-package
 flymake
 :ensure nil
 :hook (prog-mode . flymake-mode)
 :config
 (set-face-attribute
  'flymake-error nil
  :underline '(:style wave :color "#F28FAD"))
 (set-face-attribute
  'flymake-warning nil
  :underline '(:style wave :color "#FAE3B0"))
 (set-face-attribute
  'flymake-note nil
  :underline '(:style wave :color "#96CDFB"))
 :general
 (:states
  'normal
  "[d"
  'flymake-goto-prev-error
  "]d"
  'flymake-goto-next-error))

(provide 'config-diagnostics)

;;; config-diagnostics.el ends here
