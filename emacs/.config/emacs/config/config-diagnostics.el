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
 (set-face-attribute 'flymake-error nil
                     :underline '(:style wave :color "#F28FAD"))
 (set-face-attribute 'flymake-warning nil
                     :underline '(:style wave :color "#FAE3B0"))
 (set-face-attribute 'flymake-note nil
                     :height 0.6
                     :underline '(:style wave :color "#96CDFB"))

 :general
 (:states
  'normal
  "[d"
  'flymake-goto-prev-error
  "]d"
  'flymake-goto-next-error))

(use-package
 flymake-eslint
 :custom (flymake-eslint-prefer-json-diagnostics t)
 :config
 (defun custom/use-local-eslint ()
   (interactive)
   (let* ((root
           (locate-dominating-file (buffer-file-name) "node_modules"))
          (eslint
           (and root
                (expand-file-name "node_modules/.bin/eslint" root))))
     (when (and eslint (file-executable-p eslint))
       (setq-local flymake-eslint-executable-name eslint)
       (message (format "Found local ESLINT! Setting: %s" eslint))
       (flymake-eslint-enable))))

 (add-hook 'eglot-managed-mode-hook #'custom/use-local-eslint))

(use-package
 flymake-ruff
 :hook (eglot-managed-mode . flymake-ruff-load))

(provide 'config-diagnostics)

;;; config-diagnostics.el ends here
