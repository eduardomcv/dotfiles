;;; config-treesitter.el --- Tree-sitter configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configure tree-sitter grammars.

;;; Code:

(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.yml\\'" . yaml-ts-mode)
         ("\\Dockerfile\\'" . dockerfile-ts-mode))
  :preface
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")))
  :custom
  (treesit-font-lock-level 4)
  (major-mode-remap-alist
   '((python-mode . python-ts-mode)
     (javascript-mode . js-ts-mode)
     (js-json-mode . json-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (conf-toml-mode . toml-ts-mode)
     (bash-mode . bash-ts-mode)
     (sh-mode . bash-ts-mode)
     (css-mode . css-ts-mode)
     (json-mode . json-ts-mode)
     (html-mode . html-ts-mode))))

(defun custom/install-treesit-grammars ()
  "Install all tree-sitter grammars defined in `treesit-language-source-alist`."
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (treesit-install-language-grammar (car grammar))))

(provide 'config-treesitter)

;;; config-treesitter.el ends here
