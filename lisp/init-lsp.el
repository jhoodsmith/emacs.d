;;; init-lsp.el --- Eglot, Flymake and tree-sitter  -*- lexical-binding: t -*-

;;; Commentary:

;; Language-independent IDE features.  Per-language server programs are
;; registered in the init-lang-*.el modules.

;;; Code:

;;;; Flymake

(require 'flymake)
(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'text-mode-hook #'flymake-mode)

;; Provide some flycheck-like bindings in flymake mode.
(define-key flymake-mode-map (kbd "C-c ! n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "C-c ! p") #'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "C-c ! c") #'flymake-start)

;;;; Eglot

(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format))
  :config
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-stay-out-of '(eldoc)))

(use-package consult-eglot)

;;;; Tree-sitter

;; brew install tree-sitter
(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  (treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  :init
  (push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
  (push '(js-mode . js-ts-mode) major-mode-remap-alist)
  (push '(java-mode . java-ts-mode) major-mode-remap-alist)
  (push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
  (push '(ruby-mode . ruby-ts-mode) major-mode-remap-alist)
  (push '(python-mode . python-ts-mode) major-mode-remap-alist)
  (push '(go-mode . go-ts-mode) major-mode-remap-alist))

;; To install all language grammars:
;;
;;   (mapc #'treesit-install-language-grammar
;;         (mapcar #'car treesit-language-source-alist))
;;
;; Building can fail; if so, clone https://github.com/casouri/tree-sitter-module,
;; run ./batch.sh, then: mv dist/* ~/.config/emacs/tree-sitter

(provide 'init-lsp)
;;; init-lsp.el ends here
