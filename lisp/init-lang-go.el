;;; init-lang-go.el --- Go  -*- lexical-binding: t -*-

;;; Commentary:

;; Go using the tree-sitter mode, formatting on save with goimports.

;;; Code:

(use-package go-ts-mode
  :ensure nil
  :hook
  (go-ts-mode . go-format-on-save-mode)
  :bind (:map go-ts-mode-map
              ("C-c C-c" . compile))
  :config
  (reformatter-define go-format
    :program "goimports"
    :args '("/dev/stdin")))

(use-package go-playground
  :bind (:map go-playground-mode-map
              ("C-c C-c" . go-playground-exec)))

(provide 'init-lang-go)
;;; init-lang-go.el ends here
