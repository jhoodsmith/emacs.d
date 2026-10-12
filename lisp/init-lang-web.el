;;; init-lang-web.el --- HTML, JavaScript, TypeScript and friends  -*- lexical-binding: t -*-

;;; Commentary:

;; Web languages and data formats.

;;; Code:

(use-package sgml-mode
  :ensure nil
  :hook
  (html-mode . sgml-electric-tag-pair-mode)
  (html-mode . sgml-name-8bit-mode)
  :custom
  (sgml-basic-offset 2))

(use-package js
  :ensure nil
  :custom
  (js-indent-level 2))

(use-package nodejs-repl
  :bind (:map js-mode-map
              ("C-x C-e" . nodejs-repl-send-last-expression)
              ("C-c C-r" . nodejs-repl-send-region)))

(use-package typescript-ts-mode
  :ensure nil
  :config
  (reformatter-define prettier-format
    :program "npx"
    :args (list "prettier" "--stdin-filepath" (buffer-file-name)))
  :mode
  ((rx ".mjs" eos) . typescript-ts-mode)
  ((rx ".ts" eos) . typescript-ts-mode)
  ((rx ".tsx" eos) . tsx-ts-mode)
  :hook
  (tsx-ts-mode . prettier-format-on-save-mode)
  (typescript-ts-mode . prettier-format-on-save-mode)
  (tsx-ts-mode . sgml-electric-tag-pair-mode))

(use-package json-mode)
(use-package graphql-mode)

;; HTTP requests: .rest and .http files
(use-package restclient
  :mode (((rx ".rest" eos) . restclient-mode)
         ((rx ".http" eos) . restclient-mode)))

(provide 'init-lang-web)
;;; init-lang-web.el ends here
