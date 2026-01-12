;;; config-dired.el --- Dired configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure and improve the Dired file browser.

;;; Code:

(use-package dired
  :ensure nil
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-mouse-drag-files t)
  (dired-listing-switches
   (if (eq system-type 'darwin)
       "-lhA"
     "-lhA --group-directories-first"))
  :config
  (with-eval-after-load 'dired
    (require 'evil-collection-dired)
    (evil-collection-dired-setup))
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "TAB" 'dired-toggle-read-only
   "n"   'dired-create-empty-file
   "N"   'dired-create-directory
   "h"   'dired-up-directory
   "l"   'dired-find-file)
  (general-define-key
   :states 'normal
   :keymaps 'wdired-mode-map
   "="   'wdired-finish-edit
   "ESC" 'wdired-abort-changes)
  (custom/leader-keys
    "." '(dired-jump :which-key "dired")))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'config-dired)

;;; config-dired.el ends here
