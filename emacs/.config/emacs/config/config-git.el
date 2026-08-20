;;; config-git.el --- Git workspace configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configures Magit client and diff highlights near the number column.

;;; Code:

(use-package
 with-editor
 :config
 ;; Fix for "Cannot determine a suitable Emacsclient" on MacOS
 (unless (executable-find "emacsclient")
   (let ((client-path
          (expand-file-name "bin/emacsclient" invocation-directory)))
     (when (file-exists-p client-path)
       (setq with-editor-emacsclient-executable client-path)))))

(use-package
 magit
 :commands magit-status
 :after xterm-color
 :custom
 (magit-display-buffer-function
  #'magit-display-buffer-same-window-except-diff-v1)
 :config (add-hook 'magit-process-mode-hook #'compilation-minor-mode)
 :general
 (custom/leader-key
  "gg"
  '(magit-status :which-key "status")
  "gl"
  '(magit-log-current :which-key "log")
  "gb"
  '(magit-blame :which-key "blame")))

(use-package
 diff-hl
 :hook
 ((dired-mode . diff-hl-dired-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

 :init (global-diff-hl-mode)

 :custom
 (diff-hl-margin-symbols-alist
  '((insert . "│")
    (delete . "│")
    (change . "│")
    (unknown . "│")
    (ignored . "│")))

 :config (diff-hl-flydiff-mode)

 (set-face-attribute 'diff-hl-insert nil
                     :inherit 'diff-added
                     :background 'unspecified)
 (set-face-attribute 'diff-hl-delete nil
                     :inherit 'diff-removed
                     :background 'unspecified)
 (set-face-attribute 'diff-hl-change nil
                     :inherit 'diff-changed
                     :background 'unspecified))

(provide 'config-git)

;;; config-git.el ends here
