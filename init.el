;;; init.el -- My Emacs configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 James Hood-Smith
;; Author: James Hood-Smith <james@hood-smith.co.uk>
;; URL: https://github.com/jhoodsmith/emacs.d

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This is my personal Emacs configuration.

;;; Code:

;; Emacs version

(let ((minver "29"))
  (when (version< emacs-version minver)
    (error "You're running Emacs %s. This config requires version %s or higher"
           emacs-version
           minver)))

;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq elisp-flymake-byte-compile-load-path load-path)

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure 't)

;; Global keybindings
(if (eq system-type 'darwin)
 (global-set-key (kbd "M-3") (lambda () (interactive) (insert "#"))))

;; Personal configuration
(setq-default
 user-full-name "James Hood-Smith"
 user-mail-address "james@hood-smith.me.uk"
 ispell-dictionary "british")

;; Some basic preferences
(setq-default
 fill-column 80
 case-fold-search t
 column-number-mode t
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 truncate-lines nil)

;; Elisp Demos
(use-package elisp-demos
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))

;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Line numbering
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Exec path from shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; Dictionary
(use-package osx-dictionary)


;; Switch Window
(use-package switch-window
  :config
  (setq-default switch-window-shortcut-style 'alphabet)
  :bind (("M-o" . switch-window)
         ("C-x 1" . switch-window-then-maximize)
         ("C-x 2" . switch-window-then-split-below)
         ("C-x 3" . switch-window-then-split-right)
         ("C-x 0" . switch-window-then-delete)
         ("C-x 4 d" . switch-window-then-dired)
         ("C-x 4 f" . switch-window-then-find-file)
         ("C-x 4 0" . switch-window-then-kill-buffer)))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook prog-mode)

(defun my/flash-mode-line ()
  "Visual bell."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq visible-bell nil ring-bell-function 'my/flash-mode-line)

;; Keyboard-centric user interface
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

;; Separate internal configuration
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Moving and duplicating lines of rectangles
(use-package move-dup
  :bind (([M-up] . move-dup-move-lines-up)
         ([M-down]  . move-dup-move-lines-down))
  :init
  (add-hook 'after-init-hook 'move-dup-mode))

;; Beacon for never losing cursor
(use-package beacon
  :diminish
  :init
  (add-hook 'after-init-hook 'beacon-mode))

;; Theme
;; (use-package material-theme
;;   :config (load-theme 'material t))
(use-package dracula-theme
  :config (load-theme 'dracula t))

;; Diminish
(use-package diminish)

;; Company mode
(use-package company
  :diminish company-mode
  :init
  (setq tab-always-indent 'complete)
  ;; Navigate in completion minibuffer with `C-n` and `C-p`.
  :bind (([remap completion-at-point] . company-complete)
         ([remap indent-for-tab-command] . company-indent-or-complete-common)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  
  :config
  ;; Use company mode everywhere.
  (global-company-mode t))

(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode))

;; Flymake
(require 'flymake)
(add-hook 'prog-mode-hook 'flymake-mode)
(add-hook 'text-mode-hook 'flymake-mode)

;; Provide some flycheck-like bindings in flymake mode
(define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "C-c ! c") 'flymake-start)

;; Which key for displaying keybindings
(use-package which-key
  :diminish
  :init
  (add-hook 'after-init-hook 'which-key-mode))

;; Magit
(use-package magit
  :bind (("C-x g" . magit-status))
  :custom
  (magit-repository-directories
   '(;; Directory and depth to search
     ("~/work/"      . 1)
     ("~/org/"      . 0)
     ("~/.emacs.d/" . 0))))

(use-package git-timemachine
  :bind ("C-x v t" . git-timemachine-toggle))

;; Projectile
(use-package projectile
  :diminish
  :init
  (add-hook 'after-init-hook 'projectile-mode)
  (require 'magit)
  (mapc #'projectile-add-known-project
        (mapcar #'file-name-as-directory (magit-list-repos)))
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package projectile-rails
  :bind-keymap
  ("C-c r" . projectile-rails-command-map)
  :config
  (projectile-rails-global-mode))

;; Vertical display in minibuffer
(use-package vertico
  :init
  (add-hook 'after-init-hook 'vertico-mode))

;; orderless completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Do not use Orderless with Company
(defun my/company-completion-styles (capf-fn &rest args)
  (let ((completion-styles '(basic partial-completion)))
    (apply capf-fn args)))
(advice-add 'company-capf :around #'my/company-completion-styles)

;; Search and navigation
(use-package consult
  :bind
  (([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap goto-line] . consult-goto-line))
  :custom
  (consult-project-root-function 'projectile-project-root))

;; brew install aspell
(use-package flyspell
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

;; markdown
(use-package markdown-mode)

;; Edit regions in separate buffers
(use-package edit-indirect)

;; Org
(defun my/new-org-note ()
  "Create a new note in ~/org/inbox.org."
  (interactive)
  (org-capture nil "n"))

(global-set-key (kbd "C-c n") #'my/new-org-note)

(setq-default org-confirm-babel-evaluate nil
              org-image-actual-width nil
              org-startup-with-inline-images t
              org-src-window-setup 'other-window
              org-src-fontify-natively t
              org-babel-python-command "python3"
              org-agenda-files (directory-files-recursively "~/org" "\\.org$")
              org-capture-templates '(("n" "Note" entry (file "~/org/inbox.org")
                                       "* %?\nEntered on %U\n  %i\n  %a")))

;; New version of org-bullets - for making headings and lists look nicer
(use-package org-superstar
  :custom
  (org-superstar-item-bullet-alist
   '((?* . ?•)
     (?+ . ?➤)
     (?- . ?•)))
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

(use-package ob-restclient)

(use-package mermaid-mode)

;; Requires mermaid-cli: npm install -g @mermaid-js/mermaid-cli
(use-package ob-mermaid
  :custom
  (ob-mermaid-cli-path "~/.asdf/shims/mmdc"))

;; Converting Jupyter notebooks
(use-package code-cells
  :custom
  (code-cells-convert-ipynb-style '(("pandoc" "--to" "ipynb" "--from" "org")
	                            ("pandoc" "--to" "org" "--from" "ipynb")
                                    org-mode)))
(use-package ob-go)

(setq-default org-ditaa-jar-path "/opt/homebrew/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar")

;; Org presentations
(use-package org-tree-slide
  :bind (:map org-mode-map
              ("s-p" . org-tree-slide-mode)
              ("s-<return>" . org-insert-item)
              :map org-tree-slide-mode-map
              ("<left>" . org-tree-slide-move-previous-tree)
              ("<right>" . org-tree-slide-move-next-tree))
  :hook ((org-tree-slide-play . (lambda ()
                                  (text-scale-increase 2)
                                  (org-display-inline-images)
                                  (read-only-mode 1)))
         (org-tree-slide-stop . (lambda ()
                                  (text-scale-increase 0)
                                  (org-remove-inline-images)
                                  (read-only-mode -1))))
  :custom
  (org-tree-slide-header nil))

(use-package gnuplot)

;; Enable languages for Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (awk . t)
   (clojure . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (go . t)
   (haskell . t)
   (java . t)
   (latex . t)
   (mermaid . t)
   (plantuml . t)
   (python . t)
   (restclient . t)
   (ruby . t)
   (shell . t)
   (sql . t)))

;; Plantuml
(use-package plantuml-mode
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
  (org-plantuml-jar-path (expand-file-name "~/bin/plantuml.jar")))

(use-package pytest)

;; Python
(use-package python-mode
  :ensure nil
  :bind (:map python-ts-mode-map
              ("C-c C-t t" . pytest-one)))

(use-package pyvenv
  :config
  (add-hook 'python-mode-hook 'pyvenv-mode)
  (add-hook 'python-mode-hook 'pyvenv-tracking-mode))


;; Export to Hugo
(use-package ox-hugo)

;; Copies selected regions in org-mode and transforms to formatted output
;; use ox-clip-formatted-copy
(use-package ox-clip)

;; Docker
(use-package dockerfile-mode)
(use-package docker-compose-mode)

;; Restclient
(use-package restclient
  :mode (((rx ".rest" eos) . restclient-mode)
         ((rx ".http" eos) . restclient-mode)))

;; LaTeX
(use-package tex
  :ensure auctex)

;; Vterm
(use-package vterm
  :custom (vterm-install t))

;; Switch Java
(require 'switch-java)

;; Eglot
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

(with-eval-after-load 'eglot
  ;;(add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) . ("localhost" 7658))))
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) . ("bundle" "exec" "solargraph" "stdio")))
  (add-to-list 'eglot-server-programs
                       `((rust-mode rust-ts-mode) . ("rust-analyzer" :initializationOptions
                                                     ( :procMacro (:enable t)
                                                       :cargo ( :buildScripts (:enable t)
                                                                :features "all"))))))


;; Ruby
(defun my/ruby-set-lsp-config ()
  "Load LSP config from solargraph.json."
  (setq-default eglot-workspace-configuration
              (let* ((config-file (file-name-concat user-emacs-directory "lsp-config" "solargraph.json")))
                (with-temp-buffer
                  (insert-file-contents config-file)
                  (json-parse-buffer :object-type 'plist :false-object :json-false)))))

(add-hook 'ruby-mode-hook #'my/ruby-set-lsp-config)
(add-hook 'ruby-ts-mode-hook #'my/ruby-set-lsp-config)

(use-package inf-ruby
  :config
  (defun my/rails-console ()
    "Run a Rails console."
    (interactive)
    (inf-ruby-console-run "bundle exec rails c -- --nomultiline" "rails"))
  
  (defun my/bundler-console ()
    "Run a Rails console."
    (interactive)
    (inf-ruby-console-run "bin/console" "ruby")))

(use-package ruby-mode
  :ensure nil
  :bind (:map ruby-mode-map
              ("C-c C-c" . ruby-send-buffer)))

(use-package slim-mode)
(use-package rspec-mode)
(use-package rubocop)

;; ASDF
(require 'asdf)
(asdf-enable)
(setq asdf-binary "/opt/homebrew/opt/asdf/bin/asdf")

;; Terraform
(use-package terraform-mode)
(use-package company-terraform
  :hook (terraform-mode . company-terraform-init))

;; Silver surfer
(use-package ag)

;; SQL
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

;; Yasnippet
(use-package yasnippet
  :bind
  ("C-c y s" . yas-insert-snippet)
  ("C-c y v" . yas-visit-snippet-file)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;; Web browser
(use-package eww
  :commands eww eww-follow-link
  :init
  ;; (setq browse-url-browser-function 'eww-browse-url)
  (setq eww-search-prefix "http://www.google.com/search?q=")

  (defun eww-wiki (text)
    "Function used to search wikipedia for the given text."
    (interactive (list (read-string "Wiki for: ")))
    (eww (format "https://en.m.wikipedia.org/wiki/Special:Search?search=%s"
                 (url-encode-url text))))

  :bind (("C-c w w" . eww)
         ("C-c w i" . eww-wiki)
         ("C-c w l" . eww-follow-link)))

;; Clojure
(use-package clojure-mode)
(use-package cider)

;; Install deps-new
;; clojure -Ttools install-latest :lib io.github.seancorfield/deps-new :as new
;; check if it's installed
;; clojure -Ttools list
(use-package clj-deps-new)

(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
         (clojure-mode . paredit-mode)
         (clojurescript-mode . paredit-mode)
         (cider-repl-mode . paredit-mode))
  :bind (:map paredit-mode-map ("RET" . nil)))
(provide 'init)

;; kotlin
(use-package kotlin-mode)

;; Emacs package for working with Graphviz DOT-format files
(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

;; Node
(use-package nodejs-repl
  :bind (:map js-mode-map
         ("C-x C-e" . nodejs-repl-send-last-expression)
         ("C-c C-r" . nodejs-repl-send-region)))

;; Tree sitter
;; brew install tree-sitter

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  :init
  (push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
  (push '(js-mode . js-ts-mode) major-mode-remap-alist)
  (push '(java-mode . java-ts-mode) major-mode-remap-alist)
  (push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
  (push '(ruby-mode . ruby-ts-mode) major-mode-remap-alist)
  (push '(python-mode . python-ts-mode) major-mode-remap-alist)
  (push '(go-mode . go-ts-mode) major-mode-remap-alist)
  :custom
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
     (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

;; To install all language grammars

;; (mapc #'treesit-install-language-grammar (mapcar #'car
;; treesit-language-source-alist)) Although you might have to download them
;; manually: Clone https://github.com/casouri/tree-sitter-module, run
;; ./batch.sh, then do mv dist/* ~/.config/emacs/tree-sitter

;; Go

(use-package reformatter)

(use-package go-ts-mode
  :hook
  (go-ts-mode . go-format-on-save-mode)
  :config
  (reformatter-define go-format
    :program "goimports"
    :args '("/dev/stdin")))

(use-package go-playground
  :bind (:map go-playground-mode-map
              ("C-c C-c" . go-playground-exec)))

;; Rust
;; brew install rust rust-analyzer
;; cargo new hello_world --bin
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))


;; HTML
(use-package sgml-mode
  :ensure nil
  :hook
  (html-mode . sgml-electric-tag-pair-mode)
  (html-mode . sgml-name-8bit-mode)
  :custom
  (sgml-basic-offset 2))

;; Javascript
(use-package js
  :ensure nil
  :custom
  (js-indent-level 2))

;; JSON
(use-package json-mode)

;; GraphQL
(use-package graphql-mode)

;; Typescript
(use-package typescript-ts-mode
  :hook
  (tsx-ts-mode . sgml-electric-tag-pair-mode)
  :mode
  ((rx ".mjs" eos) . typescript-ts-mode)
  ((rx ".ts" eos) . typescript-ts-mode)
  ((rx ".tsx" eos) . tsx-ts-mode))

;; Copilot
;; (use-package editorconfig)

;; (use-package copilot
;;   :load-path (lambda () (expand-file-name "copilot.el" user-emacs-directory))
;;   :commands (global-copilot-mode)
;;   :diminish
;;   :bind (:map copilot-mode-map
;;          ("M-n" . copilot-next-completion)
;;          ("M-p" . copilot-previous-completion)
;;          ("M-<return>" . copilot-accept-completion))
;;   :hook
;;   (text-mode . copilot-mode)
;;   (prog-mode . copilot-mode))

(use-package gptel
  :bind ("C-c g" . gptel-send)
  :config
  (defvar gptel-backend-anthropic
    (gptel-make-anthropic "Claude" :key (auth-source-pick-first-password :host "api.anthropic.com")))
  (setf (alist-get 'default gptel-directives) "You are a large language model living in Emacs and a helpful assistant. Respond concisely. Put any mathematical expression or equation within a latex fragment so that it can be previewed in org mode.")
  (setq gptel-default-mode 'org-mode
        gptel-backend gptel-backend-anthropic))

;; gptel
(require 'gptel-quick)

(use-package kubernetes)


;; Configure warnings
(setq-default warning-minimum-level :error)

;;; init.el ends here
