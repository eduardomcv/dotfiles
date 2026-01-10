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

  (defun custom/get-eldoc-window ()
    (seq-find (lambda (w)
                (string-prefix-p "*eldoc" (buffer-name (window-buffer w))))
              (window-list)))

  (defun custom/close-eldoc-window-on-next-command ()
    (let ((win (custom/get-eldoc-window)))
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
                              handle-switch-frame
                              mwheel-scroll
                              scroll-up-command
                              scroll-down-command)))
            (delete-window win))
        (remove-hook 'pre-command-hook #'custom/close-eldoc-window-on-next-command))))

  (defun custom/toggle-eldoc-tooltip ()
    (interactive)
    (let ((win (custom/get-eldoc-window)))
      (if win
          (progn
            (delete-window win)
            (remove-hook 'pre-command-hook #'custom/close-eldoc-window-on-next-command))
        (eldoc-doc-buffer t)
        (add-hook 'pre-command-hook #'custom/close-eldoc-window-on-next-command)))))

(provide 'config-eldoc)

;;; config-eldoc.el ends here
