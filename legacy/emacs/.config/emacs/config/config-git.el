;;; config-git.el --- Git workspace configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configures Magit client and diff highlights near the number column.

;;; Code:

(use-package
 with-editor
 :config
 ;; Fix for "Cannot determine a suitable Emacsclient" on MacOS
 (unless (executable-find "emacsclient")
   (let ((client-path (expand-file-name "bin/emacsclient" invocation-directory)))
     (when (file-exists-p client-path)
       (setq with-editor-emacsclient-executable client-path)))))

(use-package
 magit
 :commands magit-status
 :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
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
 xterm-color
 :after magit
 :config
 (defun custom/magit-process-filter-advice (orig-fn proc string)
   (funcall orig-fn proc (xterm-color-filter string)))

 (advice-add 'magit-process-filter :around #'custom/magit-process-filter-advice)

 (advice-add
  'magit-start-process
  :around
  (lambda (orig-fun &rest args)
    (let ((process-environment (append process-environment '("FORCE_COLOR=1" "TERM=xterm-256color"))))
      (apply orig-fun args)))))

(use-package
 diff-hl
 :hook
 ((dired-mode . diff-hl-dired-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

 :init (global-diff-hl-mode)

 :custom (diff-hl-margin-symbols-alist '((insert . "│") (delete . "│") (change . "│") (unknown . "│") (ignored . "│")))

 :config (diff-hl-flydiff-mode)

 (set-face-attribute 'diff-hl-insert nil :inherit 'diff-added :background 'unspecified)
 (set-face-attribute 'diff-hl-delete nil :inherit 'diff-removed :background 'unspecified)
 (set-face-attribute 'diff-hl-change nil :inherit 'diff-changed :background 'unspecified)

 :general
 (:states 'normal "] h" 'diff-hl-next-hunk "[ h" 'diff-hl-previous-hunk)
 (custom/leader-key
  "gh" '(diff-hl-show-hunk :which-key "show hunk") "gH" '(diff-hl-revert-hunk :which-key "revert hunk")))

(use-package
 smerge-mode
 :ensure nil
 :demand t
 :config
 (defun custom/smerge-auto-enable ()
   "Turn on `smerge-mode' if the buffer contains conflict markers."
   (save-excursion
     (goto-char (point-min))
     (when (re-search-forward "^<<<<<<< " nil t)
       (smerge-mode 1))))

 (add-hook 'find-file-hook #'custom/smerge-auto-enable)
 ;; Enabled from `find-file-hook', after evil has already published its
 ;; auxiliary keymaps, so `smerge-mode-map' bindings need a re-normalize.
 (add-hook 'smerge-mode-hook #'evil-normalize-keymaps)

 :general (:states 'normal "] x" 'smerge-next "[ x" 'smerge-prev)
 (custom/leader-key
  :keymaps
  'smerge-mode-map
  "gco"
  '(smerge-keep-upper :which-key "keep ours")
  "gct"
  '(smerge-keep-lower :which-key "keep theirs")
  "gcb"
  '(smerge-keep-all :which-key "keep both")))

(provide 'config-git)

;;; config-git.el ends here
