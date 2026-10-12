;;; init-lang-misc.el --- Other languages and formats  -*- lexical-binding: t -*-

;;; Commentary:

;; Smaller language setups that don't warrant a module of their own.

;;; Code:

(use-package kotlin-mode)

;; Graphviz DOT files
(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

;; SQL
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

;; LaTeX
(use-package tex
  :ensure auctex)

(provide 'init-lang-misc)
;;; init-lang-misc.el ends here
