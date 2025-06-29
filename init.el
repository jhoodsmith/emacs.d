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

;; Line numbering
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Exec path from shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-shell-name "/bin/zsh")
  (setq exec-path-from-shell-variables
        (append exec-path-from-shell-variables '("NODE_EXTRA_CA_CERTS" "DIGITALOCEAN_ACCESS_TOKEN")))
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

(use-package all-the-icons
  :if (display-graphic-p))

(use-package treemacs
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

;; Optional but recommended packages
(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

;; Icons for treemacs (optional but nice)
(use-package treemacs-all-the-icons
  :after treemacs
  :config
  (treemacs-load-theme "all-the-icons"))

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
(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :custom
  ;; Edit these settings after trying the defaults
  (org-modern-star '("◉" "○" "●" "○" "●" "○" "●"))
  (org-modern-list '((43 . "➤") (45 . "–") (42 . "•")))
  (org-modern-tag nil)
  (org-modern-priority nil)
  (org-modern-todo nil)
  (org-modern-table t)
  ;; Fixes
  (org-modern-block-fringe nil)
  (org-modern-block-name nil)
  (org-modern-horizontal-rule nil)
  :config
  ;; Choose which elements to prettify
  (setq org-modern-hide-stars nil)  ;; Keep the asterisks visible
  
  ;; Optional: add some padding in org-mode buffers
  (add-hook 'org-mode-hook #'(lambda ()
                               (setq left-margin-width 2)
                               (setq right-margin-width 2)
                               (set-window-buffer nil (current-buffer)))))

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

(defun my/get-historical-weather (latitude longitude timestamp)
  "Get historical weather from openweathermap.org with metric units.
Arguments:
  LATITUDE: latitude coordinate of the location
  LONGITUDE: longitude coordinate of the location
  TIMESTAMP: Unix timestamp for the historical data."
  (let* ((url-request-method "GET")
         (api-key (auth-source-pick-first-password :host "api.openweathermap.org"))
         (url (format "https://api.openweathermap.org/data/3.0/onecall/timemachine?units=metric&lat=%s&lon=%s&dt=%s&appid=%s"
                      latitude longitude timestamp api-key)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point-min) (point))
      (buffer-string))))

(use-package mcp
  :load-path "mcp.el/"
  :config (require 'mcp-hub)
  :custom
  (mcp-hub-servers `(("github"
                      :command "github-mcp-server"
                      :args ("stdio")
                      :env (:GITHUB_PERSONAL_ACCESS_TOKEN ,(auth-source-pick-first-password :host "api.github.com")))
                     ("figma"
                      :command "npx"
                      :args ("-y" "figma-developer-mcp" "--stdio" ,(concat "--figma-api-key=" (auth-source-pick-first-password :host "api.figma.com"))))
                     ("aws-documentation-mcp-server"
                      :command "uvx"
                      :args ("awslabs.aws-documentation-mcp-server@latest")
                      :env (:FASTMCP_LOG_LEVEL "ERROR" :AWS_DOCUMENTATION_PARTITION "aws"))
                     ("filesystem"
                      :command "mcp-filesystem-server"
                      :args ("/Users/james.hood-smith/work" "/Users/james.hood-smith/scratch")))))

(use-package gptel
  :bind (("C-c g s" . gptel-send)
         ("C-c g a" . gptel-add)
         ("C-c g m" . gptel-menu)
         ("C-c g g" . gptel)
         ("C-c g w" . gptel-rewrite)
         ("C-c g r" . gptel-context-remove-all))
  :config
  (require 'gptel-integrations)

  (defvar gptel-backend-bedrock
    (gptel-make-bedrock "Bedrock"
      :region "eu-west-2"))

  (defvar gptel-backend-gemini
    (gptel-make-gemini "Gemini"
      :key (auth-source-pick-first-password :host "generativelanguage.googleapis.com")))

  (gptel-make-preset 'gemini-with-search
    :description "A preset for Gemini with web search"
    :backend "Gemini"
    :model 'gemini-2.5-flash
    :request-params '(:tools [(:google_search ()) (:url_context ())]))

  (defvar gptel-backend-gh
    (gptel-make-gh-copilot "Copilot"))

  (defvar gptel-backend-anthropic
    (gptel-make-anthropic "Claude"
      :key (auth-source-pick-first-password :host "api.anthropic.com")))

  (defvar gptel-backend-openai
    (gptel-make-openai "ChatGPT"
      :stream t
      :models gptel--openai-models
      :key (auth-source-pick-first-password :host "api.openai.com")))

  (setf (alist-get 'default gptel-directives) "You are a large language model living in Emacs and a helpful assistant. Respond concisely. Put any mathematical expression or equation within a latex fragment so that it can be previewed in org mode.")

  (setq gptel-default-mode 'org-mode
        gptel-log-level 'info
        ;; gptel-model 'claude-3.7-sonnet
        gptel-cache t
        gptel-model 'gemini-2.5-flash
        gptel-backend gptel-backend-gemini)

  (gptel-make-tool
   :name "my_run_command"
   :function (lambda (command)
               (let* ((project-root (projectile-project-root))
                      ;; Map command prefixes to their environment runners
                      (command-env-map '(("pytest" . "uv run")
                                         ("ruff" . "uv run")
                                         ("mypy" . "uv run")
                                         ("rspec" . "bundle exec")
                                         ("rubocop" . "bundle exec")))
                      (cmd-name (car (split-string command)))
                      (cmd-entry (assoc cmd-name command-env-map)))
                 (if cmd-entry
                     (let* ((default-directory project-root)
                            (env-prefix (cdr cmd-entry))
                            (full-command (format "%s %s" env-prefix command)))
                       (with-temp-buffer
                         (let ((exit-code (call-process-shell-command full-command nil t nil)))
                           (let ((output (buffer-string)))
                             (format "Command: %s\nExit code: %d\nOutput:\n%s" 
                                     full-command exit-code output)))))
                   (format "Error: Command '%s' is not in the whitelist. Allowed commands: %s" 
                           cmd-name (mapconcat #'car command-env-map ", ")))))
   :description "Run whitelisted development commands from the project root. Python commands run with uv run, Ruby commands with bundle exec."
   :args (list '(:name "command"
                       :type string
                       :description "The command to run. Available commands: pytest, ruff, mypy (Python); rspec, rubocop (Ruby). Arguments can be added after the command."))
   :category "development")

  ;; gptel tool for getting current time
  (gptel-make-tool
   :name "current_time"
   :function (lambda () (format-time-string "%Y-%m-%d %H:%M:%S"))
   :description "Returns current time in the format %Y-%m-%d %H:%M:%S"
   :category "general")

  ;; gptel tool for getting historical weather data
  (gptel-make-tool
   :name "get_historical_weather"
   :function (lambda (latitude longitude timestamp)
               (my/get-historical-weather latitude longitude timestamp))
   :description "Get historical weather data from OpenWeatherMap API. All units of measurement in the response are metric."
   :args (list '(:name "latitude"
                       :type string
                       :description "Latitude coordinate of the location")
               '(:name "longitude"
                       :type string
                       :description "Longitude coordinate of the location")
               '(:name "timestamp"
                       :type string
                       :description "Unix timestamp for the historical data"))
   :category "weather")

  (gptel-make-tool
   :name "my_create_file"
   :function (lambda (rel-path filename content)
               (let* ((project-root (projectile-project-root))
                      (full-path (expand-file-name (concat (file-name-as-directory rel-path) filename)
                                                   project-root)))
                 (with-temp-buffer
                   (insert content)
                   (write-file full-path))
                 (format "Created file %s in project path %s" filename rel-path)))
   :description "Create a new file with the specified content (relative to project root)"
   :args (list '(:name "rel-path"
                       :type string
                       :description "The directory path relative to project root (e.g., '.' or 'src/util')")
               '(:name "filename"
                       :type string
                       :description "The name of the file to create")
               '(:name "content"
                       :type string
                       :description "The content to write to the file"))
   :category "filesystem")

  (gptel-make-tool
   :name "my_list_directory"
   :function (lambda (rel-path &optional match full)
               (let* ((project-root (projectile-project-root))
                      (dir (expand-file-name rel-path project-root))
                      (files (directory-files dir full match)))
                 (mapconcat #'identity files "\n")))
   :description "List the contents of a directory (relative to project root)"
   :args (list '(:name "rel-path"
                       :type string
                       :description "The directory path relative to project root (e.g., '.' or 'src/util')")
               '(:name "match"
                       :type string
                       :description "Optional regex pattern to filter files"
                       :optional t)
               '(:name "full"
                       :type boolean
                       :description "Whether to return full pathnames"
                       :optional t))
   :category "filesystem")

  (gptel-make-tool
   :name "my_read_file"
   :function (lambda (rel-filepath &optional max-chars)
               (let* ((project-root (projectile-project-root))
                      (path (expand-file-name rel-filepath project-root)))
                 (if (file-readable-p path)
                     (with-temp-buffer
                       (insert-file-contents path)
                       (if max-chars
                           (buffer-substring-no-properties
                            (point-min)
                            (min (point-max) (+ (point-min) max-chars)))
                         (buffer-string)))
                   (format "Error: Cannot read file %s" rel-filepath))))
   :description "Read the contents of a text file (relative to project root)"
   :args (list '(:name "rel-filepath"
                       :type string
                       :description "Path to the file relative to project root (e.g., 'README.md' or 'src/main.py')")
               '(:name "max-chars"
                       :type integer
                       :description "Optional maximum number of characters to read"
                       :optional t))
   :category "filesystem")

  (gptel-make-tool
   :name "my_update_file"
   :function (lambda (rel-filepath content &optional append)
               (let* ((project-root (projectile-project-root))
                      (path (expand-file-name rel-filepath project-root)))
                 (if (file-exists-p path)
                     (with-temp-buffer
                       (when append
                         (insert-file-contents path))
                       (if append
                           (goto-char (point-max))
                         (erase-buffer))
                       (insert content)
                       (write-file path)
                       (format "Updated file %s successfully" rel-filepath))
                   (format "Error: File %s does not exist" rel-filepath))))
   :description "Update the contents of an existing file (relative to project root)"
   :args (list '(:name "rel-filepath"
                       :type string
                       :description "Path to the file relative to project root (e.g., 'README.md' or 'src/main.py')")
               '(:name "content"
                       :type string
                       :description "The content to write to the file")
               '(:name "append"
                       :type boolean
                       :description "Whether to append to the file instead of replacing content"
                       :optional t))
   :category "filesystem"))

(defun my/aws-login (profile)
  "Login to AWS SSO with PROFILE and export credentials to environment."
  (interactive (list (completing-read "AWS Profile: " 
                                     (split-string (shell-command-to-string 
                                                    "aws configure list-profiles") "\n" t))))
  (message "Logging in to AWS SSO with profile %s..." profile)
  (if (zerop (call-process "aws" nil nil nil "sso" "login" "--profile" profile))
      (condition-case err
          (let* ((json-string (shell-command-to-string
                              (format "aws configure export-credentials --profile %s" profile)))
                 (json-data (json-parse-string json-string :object-type 'plist)))
            (setenv "AWS_ACCESS_KEY_ID" (plist-get json-data :AccessKeyId))
            (setenv "AWS_SECRET_ACCESS_KEY" (plist-get json-data :SecretAccessKey))
            (setenv "AWS_SESSION_TOKEN" (plist-get json-data :SessionToken))
            (message "AWS credentials for profile %s exported to environment" profile))
        (error (message "Failed to export credentials: %s" (error-message-string err))))
    (message "AWS SSO login failed for profile %s" profile)))

(use-package kubernetes)

;; Configure warnings
(setq-default warning-minimum-level :error)

;;; init.el ends here
