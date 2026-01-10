;;; config-eldoc.el --- Eldoc configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configures documentation popups (Eldoc).

;;; Code:

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p t)

  :config
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-height . 0.20)
                 (dedicated . t)))

	(defun custom/close-eldoc-window-on-next-command ()
    "Close eldoc window on the next command (unless switching windows)."
    (let ((win (get-buffer-window "*eldoc*")))
      (if win
          (unless (or (eq (selected-window) win)
                      (memq this-command
                            '(custom/toggle-eldoc-tooltip
                              other-window
                              evil-window-down
                              evil-window-up
                              evil-window-next
                              evil-window-prev
                              evil-window-mru
                              handle-switch-frame)))
            (delete-window win))
        (remove-hook 'pre-command-hook #'custom/close-eldoc-window-on-next-command))))

  (defun custom/toggle-eldoc-tooltip ()
    "Toggle eldoc window. If visible, will auto-close on the next cursor movement."
    (interactive)
    (let ((win (get-buffer-window "*eldoc*")))
      (if win
          (progn
            (delete-window win)
            (remove-hook 'pre-command-hook 'custom/close-eldoc-window-on-next-command))
        (eldoc-doc-buffer t)
        (add-hook 'pre-command-hook 'custom/close-eldoc-window-on-next-command)))))

(provide 'config-eldoc)

;;; config-eldoc.el ends here
