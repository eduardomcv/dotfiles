;;; config-bootstrap.el --- First-boot self-setup -*- lexical-binding: t; -*-

;;; Commentary:
;;; Installs whatever is missing on first launch: the Nerd Font,
;;; tree-sitter grammars, and mason.el packages.
;;; Runs in detached child processes so it doesn't block the UI.

;;; Code:

(require 'cl-lib)

(defconst custom/mason-packages
  '("typescript-language-server"
    "css-lsp"
    "eslint-lsp"
    "html-lsp"
    "json-lsp"
    "yaml-language-server"
    "bash-language-server"
    "lua-language-server"
    "emmet-ls"
    "ruby-lsp"
    "copilot-language-server"
    "ty"
    "ruff"
    "prettier"
    "stylua"
    "shfmt"
    "markdownlint-cli2"
    "debugpy"
    "js-debug-adapter")
  "LSP/DAP/formatter/linter tooling installed via mason.el.
`pandoc' is absent: it isn't in the Mason registry, so
`config-code.el' treats it as optional instead.")

(defun custom/mason-install-missing (&optional callback)
  "Install any of `custom/mason-packages' not already present.
CALLBACK, if given, is called once with an alist of
\(PACKAGE . SUCCESS) after every pending install attempt finishes."
  (let* ((pending (seq-remove #'mason-installed-p custom/mason-packages))
         (remaining (length pending))
         (results nil))
    (if (zerop remaining)
        (when callback
          (funcall callback results))
      (message "mason: installing %d missing package(s)..." remaining)
      (dolist (pkg pending)
        (mason-install
         pkg nil nil
         (lambda (success)
           (push (cons pkg success) results)
           (unless success
             (message "mason: failed to install %s" pkg))
           (setq remaining (1- remaining))
           (when (zerop remaining)
             (message "mason: finished installing")
             (when callback
               (funcall callback results)))))))))

(defun custom/bootstrap--emacs-binary ()
  "Absolute path to the running Emacs binary (Emacs.app isn't on PATH)."
  (expand-file-name invocation-name invocation-directory))

(defun custom/bootstrap--init-files ()
  "Return (EARLY-INIT . INIT) paths for the config currently running."
  (cons
   (or (bound-and-true-p early-init-file) (locate-user-emacs-file "early-init.el"))
   (or user-init-file (locate-user-emacs-file "init.el"))))

(cl-defun
 custom/bootstrap-run-in-child (name form &key sentinel (load-init t) child-load-path)
 "Run elisp FORM in a detached child Emacs process, reporting as NAME.
Unless LOAD-INIT is nil, the child loads this same init.el first, so
FORM can use config-defined variables.  CHILD-LOAD-PATH adds a -L
directory for a child that only needs one package.  SENTINEL is
called with a success boolean once the child exits."
 (let* ((init-files (custom/bootstrap--init-files))
        (command
         (append
          (list (custom/bootstrap--emacs-binary) "--batch")
          (when child-load-path
            (list "-L" child-load-path))
          (when load-init
            (list "-l" (car init-files) "-l" (cdr init-files)))
          (list "--eval" form))))
   (message "%s: starting in the background..." name)
   (make-process
    :name (format "bootstrap-%s" name)
    :command command
    :noquery t
    :sentinel
    (lambda (_proc event)
      (when (string-match-p "\\(?:finished\\|exited\\)" event)
        (let ((success (string-prefix-p "finished" event)))
          (message "%s: %s"
                   name
                   (if success
                       "done"
                     "failed, see *Messages*"))
          (when sentinel
            (funcall sentinel success))))))))

(defun custom/bootstrap-install-nerd-font ()
  "Install the Nerd Font `nerd-icons' renders glyphs from, if missing.
Checked by file path so this works the same in GUI, TTY, or batch."
  (let* ((font-dir
          (if (eq system-type 'darwin)
              (expand-file-name "~/Library/Fonts/")
            (expand-file-name "fonts/" (or (getenv "XDG_DATA_HOME") (expand-file-name "~/.local/share/")))))
         (font-file (expand-file-name "NFM.ttf" font-dir))
         (nerd-icons-dir (file-name-directory (locate-library "nerd-icons"))))
    (when (and nerd-icons-dir (not (file-exists-p font-file)))
      ;; The child only needs the nerd-icons package itself (it downloads
      ;; the font and shells out to fc-cache), not the full config.
      (custom/bootstrap-run-in-child
       "nerd-icons font" "(progn (require 'nerd-icons) (nerd-icons-install-fonts t))"
       :load-init nil
       :child-load-path nerd-icons-dir
       :sentinel
       (lambda (success)
         (when (and success (fboundp 'custom/set-font-faces))
           (dolist (frame (frame-list))
             (with-selected-frame frame
               (custom/set-font-faces)))))))))

(defconst custom/treesit-grammar-libraries
  '((python python)
    (js javascript jsdoc)
    (typescript-ts-mode typescript tsx)
    (css-mode css)
    (html-ts-mode html)
    (json-ts-mode json)
    (yaml-ts-mode yaml)
    (toml-ts-mode toml)
    (sh-script bash)
    (ruby-ts-mode ruby)
    (lua-ts-mode lua)
    (rust-ts-mode rust)
    (dockerfile-ts-mode dockerfile))
  "Grammars compiled ahead of time, keyed by the builtin library that declares their recipe in `treesit-language-source-alist'.
Emacs 31 ships a pinned recipe for each of these inside its own `*-ts-mode' file, but only registers it once that library is loaded.")

(defun custom/bootstrap-install-treesit-grammars ()
  "Compile any tree-sitter grammar not already available.
Runs in a child process since compilation blocks the caller."
  (dolist (entry custom/treesit-grammar-libraries)
    (require (car entry)))
  (when (seq-some
         (lambda (lang) (not (treesit-language-available-p lang)))
         (mapcan (lambda (entry) (copy-sequence (cdr entry))) custom/treesit-grammar-libraries))
    (custom/bootstrap-run-in-child
     "tree-sitter grammars"
     (format "(let (failures)
        (dolist (lib '%S) (require lib))
        (dolist (lang '%S)
          (unless (treesit-language-available-p lang)
            (condition-case err
                (treesit-install-language-grammar lang)
              (error (push (cons lang (error-message-string err)) failures)))))
        (dolist (f failures) (message \"treesit: failed %%s: %%s\" (car f) (cdr f)))
        (kill-emacs (if failures 1 0)))"
             (mapcar #'car custom/treesit-grammar-libraries)
             (mapcan (lambda (entry) (copy-sequence (cdr entry))) custom/treesit-grammar-libraries)))))

(defun custom/bootstrap-run-all ()
  "Run all dependency bootsrappers."
  (custom/bootstrap-install-nerd-font)
  (custom/bootstrap-install-treesit-grammars)
  (mason-setup (custom/mason-install-missing)))

;; Skip in batch mode so smoke tests stay deterministic.
(unless noninteractive
  (add-hook 'emacs-startup-hook #'custom/bootstrap-run-all))

(provide 'config-bootstrap)

;;; config-bootstrap.el ends here
