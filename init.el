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
(setq user-full-name "James Hood-Smith"
      user-mail-address "james@hood-smith.me.uk"
      ispell-dictionary "british")

;; Some basic preferences
(setq  fill-column 80
       case-fold-search t
       column-number-mode t
       indent-tabs-mode nil
       create-lockfiles nil
       auto-save-default nil
       make-backup-files nil
       truncate-lines nil)

(defun my/delete-whitespace-forward ()
  "Dlete all whitespace characters forward until the next non-whitespace character."
  (interactive)
  (delete-region (point)
                 (progn (skip-chars-forward " \t\n\r") (point))))

(global-set-key (kbd "C-c d") 'my/delete-whitespace-forward)

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
  (setq switch-window-shortcut-style 'alphabet)
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

(setq org-confirm-babel-evaluate nil
      org-image-actual-width nil
      org-startup-with-inline-images t
      org-src-window-setup 'other-window
      org-tags-column -88
      org-src-fontify-natively t
      org-babel-python-command "python3"
      org-agenda-files (directory-files-recursively "~/org" "\\.org$")
      org-capture-templates '(("n" "Note" entry (file "~/org/learnings.org")
                               "* %?%^g\nEntered on %U\n  %i\n")))
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
  ;; (add-hook 'org-mode-hook #'(lambda ()
  ;;                              (setq left-margin-width 2)
  ;;                              (setq right-margin-width 2)
  ;;                              (set-window-buffer nil (current-buffer))))
  )

;; update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

(use-package ob-restclient)
(use-package ob-typescript)

(use-package mermaid-mode)

;; Requires mermaid-cli: npm install -g @mermaid-js/mermaid-cli
(use-package ob-mermaid
  :custom
  (ob-mermaid-cli-path "~/.asdf/s
hims/mmdc"))

;; Converting Jupyter notebooks
(use-package code-cells
  :custom
  (code-cells-convert-ipynb-style '(("pandoc" "--to" "ipynb" "--from" "org")
				    ("pandoc" "--to" "org" "--from" "ipynb" "--extract-media" "./ipynb-images/")
                                       (lambda () #'org-mode))))
(use-package ob-go)

(setq org-ditaa-jar-path "/opt/homebrew/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar")

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
   (typescript . t)
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

(defun my/uv-activate ()
  "Activate the uv venv for the current project."
  (interactive)
  (let ((venv (concat (projectile-project-root) ".venv")))
    (when (file-exists-p venv)
      (pyvenv-activate venv))))

;; (add-hook 'python-base-mode-hook #'my/uv-activate)

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

;; Helm

(define-derived-mode helm-mode yaml-mode "helm"
  "Major mode for editing kubernetes helm templates")


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
  (add-to-list 'eglot-server-programs '(helm-mode "helm_ls" "serve"))
  (add-to-list 'eglot-server-programs '((python-ts-mode python-mode) . ("uv" "run" "pylsp")))
  (add-to-list 'eglot-server-programs
               `((rust-mode rust-ts-mode) . ("rust-analyzer" :initializationOptions
                                             ( :procMacro (:enable t)
                                               :cargo ( :buildScripts (:enable t)
                                                        :features "all"))))))


;; Ruby
(defun my/ruby-set-lsp-config ()
  "Load LSP config from solargraph.json."
  (setq eglot-workspace-configuration
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

;; Ripgrep
;; brew install ripgrep
(use-package rg
  :config
  (rg-enable-default-bindings))

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
  :bind (:map go-ts-mode-map
	      ("C-c C-c" . compile))
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
  :config
  (reformatter-define prettier-format
    :program "npx"
    :args (list "prettier" "--stdin-filepath" (buffer-file-name)))
  (add-to-list 'org-src-lang-modes '("typescript" . typescript-ts))
  :mode
  ((rx ".mjs" eos) . typescript-ts-mode)
  ((rx ".ts" eos) . typescript-ts-mode)
  ((rx ".tsx" eos) . tsx-ts-mode)
  :hook
  (tsx-ts-mode . prettier-format-on-save-mode)
  (typescript-ts-mode . prettier-format-on-save-mode)
  (tsx-ts-mode . sgml-electric-tag-pair-mode))

;; Using auth-source
;; brew install gnupg
;; Write to ~/.authinfo
;; gpg -c ~/.authinfo
;; rm ~/.authinfo
;; Open in emacs and give pass phrase
;; Ensure the following is in ~/.gnupg/gpg.conf:
;;  use-agent 
;;  pinentry-mode loopback
;; Ensure the following is in ~/.gnupg/gpg-agent.conf
;;  allow-loopback-pinentry
;; Use gpg -d ~/.authinfo.gpg to decrypt from command line

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
		     ("gocardless" .
		      (:url "https://mcp.gocardless.com"))
                     ("filesystem"
                      :command "mcp-filesystem-server"
	              :args ("/Users/james.hood-smith/work" "/Users/james.hood-smith/scratch")))))

(use-package gptel
  :load-path "/Users/james.hood-smith/work/gptel"
  :bind (("C-c g s" . gptel-send)
         ("C-c g a" . gptel-add)
         ("C-c g m" . gptel-menu)
         ("C-c g g" . gptel)
         ("C-c g f" . gptel-add-file)
         ("C-c g w" . gptel-rewrite)
         ("C-c g r" . gptel-context-remove-all))
  :config
  (require 'gptel-integrations)
  (require 'gptel-bedrock)
  (push '(eu-claude-sonnet-4.5-profile . "eu.anthropic.claude-sonnet-4-5-20250929-v1:0") gptel-bedrock--model-ids)
  (push '(eu-claude-haiku-4.5-profile . "eu.anthropic.claude-haiku-4-5-20251001-v1:0") gptel-bedrock--model-ids)
  (push '(eu-claude-opus-4.6-profile . "eu.anthropic.claude-opus-4-6-v1") gptel-bedrock--model-ids)
  (push '(eu-claude-sonnet-4.6-profile . "eu.anthropic.claude-sonnet-4-6") gptel-bedrock--model-ids)

  (defvar gptel-backend-bedrock
    (gptel-make-bedrock "Bedrock"
      :stream t
      :models '(eu-claude-haiku-4.5-profile eu-claude-sonnet-4.5-profile eu-claude-opus-4.6-profile eu-claude-sonnet-4.6-profile)
      :region "eu-west-1"))

  (defvar gptel-backend-gemini
    (gptel-make-gemini "Gemini"
      :key (auth-source-pick-first-password :host "generativelanguage.googleapis.com")))

  (gptel-make-preset 'about-me
    :description "Include init.el and learnings.org as context"
    :context '(:eval (list (expand-file-name "~/.emacs.d/init.el")
				     (expand-file-name "~/work/cv/cv.tex")
				     (expand-file-name "~/org/learnings/learnings.org"))))

  (gptel-make-preset 'gemini-with-search
    :description "A preset for Gemini with web search"
    :backend "Gemini"
    :include-reasoning nil
    :model 'gemini-2.5-flash
    :system "You are a helpful assistant working within Emacs. When presenting results from web search always include the URL for each result in your response so I can verify information."
    :request-params '(:tools [(:google_search ()) (:url_context ())]))

  (defvar gptel-backend-gh
    (gptel-make-gh-copilot "Copilot"))

  (defvar gptel-backend-anthropic
    (gptel-make-anthropic "Claude"
      :stream t
      :key (auth-source-pick-first-password :host "api.anthropic.com")))

  (gptel-make-preset 'claude-with-search
    :description "A preset for Claude with web search"
    :backend "Claude"
    :model 'claude-sonnet-4-6
    :system "You are a helpful assistant working within Emacs. When presenting results from web search always give me URL so I can cross check."
    :request-params '(:tools[(:type "web_search_20260209" :name "web_search" :allowed_callers ["direct"])
			     (:type "web_fetch_20260209" :name "web_fetch"  :allowed_callers ["direct"])]))

  (gptel-make-preset 'bedrock-with-search
    :description "A preset for Bedrock with Claude 4.6 and web search"
    :backend "Bedrock"
    :model 'eu-claude-sonnet-4.6-profile
    :system "You are a helpful assistant working within Emacs. When presenting results from web search always give me URL so I can cross check. I live near London, England."
    :tools '("my_web_search" "my_fetch_webpage"))


  (defvar gptel-backend-openai
    (gptel-make-openai "ChatGPT"
      :stream t
      :models gptel--openai-models
      :key (auth-source-pick-first-password :host "api.openai.com")))

  (setf (alist-get 'default gptel-directives) "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")

  (setq gptel-default-mode 'org-mode
        gptel-log-level 'info
        gptel-model 'claude-3.7-sonnet
        gptel-cache t
        gptel-max-tokens 8192
        ;; gptel-model 'gemini-2.5-flash
        gptel-backend gptel-backend-gh)

  (gptel-make-tool
   :name "my_run_command"
   :confirm nil
   :function (lambda (command)
               (let* ((project-root (projectile-project-root))
		      (max-output 10000)
                      ;; Map command prefixes to their environment runners
                      (command-env-map '(("pytest" . "uv run")
                                         ("ruff" . "uv run")
                                         ("mypy" . "uv run")
                                         ("rspec" . "bundle exec")
                                         ("rubocop" . "bundle exec")
					 ("go" . "")
					 ("npm" . "")))
                      (cmd-name (car (split-string command)))
                      (cmd-entry (assoc cmd-name command-env-map)))
                 (if cmd-entry
                     (let* ((default-directory project-root)
                            (env-prefix (cdr cmd-entry))
                            (full-command (if (string-empty-p env-prefix)
					      command
					    (format "%s %s" env-prefix command))))
                       (with-temp-buffer
                         (let ((exit-code (call-process-shell-command full-command nil t nil)))
                           (let* ((output (buffer-string))
				 (truncated (> (length output) max-output))
				 (final-output (if truncated
						   (concat (substring output 0 max-output)
							   "\n...[truncated]")
						 output)))
                             (format "Command: %s\nExit code: %d\nOutput:\n%s" 
                                     full-command exit-code final-output)))))
                   (format "Error: Command '%s' is not in the whitelist. Allowed commands: %s" 
                           cmd-name (mapconcat #'car command-env-map ", ")))))
   :description "Run whitelisted development commands from the project root. Python commands run with uv run, Ruby commands with bundle exec."
   :args (list '(:name "command"
                       :type string
                       :description "The command to run. Available commands: pytest, ruff, mypy (Python); rspec, rubocop (Ruby); go (golang); npm (node). Arguments can be added after the command."))
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
   :name "my_create_directory"
   :function (lambda (rel-path)
               (let* ((project-root (projectile-project-root))
                      (full-path (expand-file-name rel-path project-root)))
                 (if (file-exists-p full-path)
                     (format "Directory %s already exists" rel-path)
                   (make-directory full-path t)
                   (format "Created directory %s in project" rel-path))))
   :description "Create a new directory (relative to project root)"
   :args (list '(:name "rel-path"
                       :type string
                       :description "The directory path to create relative to project root (e.g., 'src/components' or 'tests/unit')"))
   :category "file-write")

  ;; Tool for viewing project structure with tree
  (gptel-make-tool
   :name "my_tree_view"
   :function (lambda (&optional rel-path max-depth ignore-patterns)
               (let* ((project-root (projectile-project-root))
                      (target-dir (if rel-path
                                      (expand-file-name rel-path project-root)
                                    project-root))
                      (args (list "-F" "--charset=ascii"))
                      (default-directory target-dir))
                 ;; Add depth limit if specified
                 (when max-depth
                   (setq args (append args (list "-L" (number-to-string max-depth)))))
                 ;; Add ignore patterns if specified
                 (when ignore-patterns
                   (dolist (pattern (split-string ignore-patterns ","))
                     (setq args (append args (list "-I" (string-trim pattern))))))
                 ;; Add common ignores
                 (setq args (append args '("-I" "node_modules" "-I" ".git" "-I" "__pycache__" "-I" "*.pyc" "-I" "tmp")))
                 (with-temp-buffer
                   (let ((exit-code (apply #'call-process "tree" nil t nil args)))
                     (if (zerop exit-code)
                         (buffer-string)
                       (format "Error: tree command failed (exit code %d). Make sure 'tree' is installed." exit-code))))))
   :description "View project directory structure using the tree command. Automatically ignores common files like node_modules, .git, __pycache__, etc."
   :args (list '(:name "rel-path"
                       :type string
                       :description "Directory path relative to project root to view (default: project root)"
                       :optional t)
               '(:name "max-depth"
                       :type integer
                       :description "Maximum depth of directory tree to display (e.g., 2 for two levels)"
                       :optional t)
               '(:name "ignore-patterns"
                       :type string
                       :description "Comma-separated list of patterns to ignore (e.g., '*.log,temp*')"
                       :optional t))
   :category "file-read")

  ;; Search using ripgrep
  (gptel-make-tool
   :name "my_search_project"
   :function (lambda (pattern &optional file-pattern case-sensitive)
               (let* ((default-directory (projectile-project-root))
                      (case-arg (if case-sensitive "" "-i"))
                      (file-arg (if file-pattern (format "-g '%s'" file-pattern) ""))
                      (command (format "rg --no-heading --line-number --color=never %s %s '%s'" 
                                       case-arg file-arg pattern)))
		 (shell-command-to-string command)))
   :description "Search for text pattern across project files using ripgrep (rg). Returns file paths, line numbers, and matching lines."
   :args (list '(:name "pattern"
                       :type string
                       :description "Text pattern to search for (supports regex)")
               '(:name "file-pattern"
                       :type string
                       :description "Optional glob pattern to filter files (e.g., '*.py', '*.rb')"
                       :optional t)
               '(:name "case-sensitive"
                       :type boolean
                       :description "Whether search should be case-sensitive (default: false)"
                       :optional t))
   :category "file-read")


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
   :category "file-write")

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
   :category "file-read")

  (gptel-make-tool
   :name "my_read_file"
   :function (lambda (rel-filepath &optional start-line end-line)
               (let* ((project-root (projectile-project-root))
                      (path (expand-file-name rel-filepath project-root)))
		 (if (file-readable-p path)
                     (with-temp-buffer
                       (insert-file-contents path)
                       (let* ((total-lines (count-lines (point-min) (point-max)))
                              (beg (if start-line
                                       (progn (goto-char (point-min))
                                              (forward-line (1- start-line))
                                              (point))
                                     (point-min)))
                              (end (if end-line
                                       (progn (goto-char (point-min))
                                              (forward-line end-line)
                                              (point))
                                     (point-max))))
			 (buffer-substring-no-properties beg end)))
                   (format "Error: Cannot read file %s" rel-filepath))))
   :args (list '(:name "rel-filepath" :type string
                       :description "Path to the file relative to project root")
               '(:name "start-line" :type integer
                       :description "First line to read (1-indexed, inclusive)"
                       :optional t)
               '(:name "end-line" :type integer
                       :description "Last line to read (1-indexed, inclusive)"
                       :optional t))
   :description "Read the contents of a text file (relative to project root)"
   :category "file-read")

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
   :category "file-write")

  (gptel-make-tool
   :name "my_fetch_webpage"
   :function (lambda (url &optional raw)
               (with-temp-buffer
		 (condition-case err
                     (progn
                       (url-insert-file-contents url)
                       (if raw
                           ;; Return raw HTML if requested
                           (buffer-string)
			 ;; Otherwise extract readable content
			 (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
				(best-node (eww-highest-readability dom)))
                           (erase-buffer)
                           (shr-insert-document best-node)
                           (buffer-substring-no-properties (point-min) (point-max)))))
                   (error (format "Error fetching %s: %s" url (error-message-string err))))))
   :description "Fetch and extract readable text content from a webpage URL"
   :args (list '(:name "url"
                       :type string
                       :description "The URL to fetch (e.g., 'https://example.com')")
               '(:name "raw"
                       :type boolean
                       :description "Return raw HTML instead of extracted readable text"
                       :optional t))
   :category "web")

  (gptel-make-tool
   :name "my_web_search"
   :function (lambda (query &optional count)
               (let* ((api-key (auth-source-pick-first-password :host "api.search.brave.com"))
                      (url (format "https://api.search.brave.com/res/v1/web/search?q=%s&count=%d"
                                   (url-hexify-string query)
                                   (or count 5)))
                      (url-request-extra-headers 
                       `(("Accept" . "application/json")
			 ("X-Subscription-Token" . ,api-key))))
		 (with-temp-buffer
                   (url-insert-file-contents url)
                   (let* ((json (json-parse-buffer :object-type 'plist))
                          (results (plist-get (plist-get json :web) :results)))
                     (mapconcat 
                      (lambda (r)
			(format "Title: %s\nURL: %s\nDescription: %s\n"
				(plist-get r :title)
				(plist-get r :url)
				(plist-get r :description)))
                      results "\n---\n")))))
   :description "Search the web using Brave Search API."
   :args (list '(:name "query"
                       :type string
                       :description "Search query")
               '(:name "count"
                       :type integer
                       :description "Number of results (default 5)"
                       :optional t))
   :category "web")

  ;; Basic status and info
  (gptel-make-tool
   :name "git_status"
   :function (lambda ()
               (let ((default-directory (projectile-project-root)))
                 (shell-command-to-string "git status --porcelain")))
   :description "Show git status for current project"
   :category "git")

  (gptel-make-tool
   :name "git_branch"
   :function (lambda ()
               (let ((default-directory (projectile-project-root)))
                 (shell-command-to-string "git branch -v")))
   :description "Show current branch and recent commit info"
   :category "git")

  ;; View changes
  (gptel-make-tool
   :name "git_diff"
   :function (lambda (&optional file-path staged ref)
               (let* ((default-directory (projectile-project-root))
                      (staged-flag (if staged "--cached" ""))
                      (ref-arg (or ref ""))
                      (file-arg (if file-path (format "-- %s" file-path) ""))
                      (command (format "git diff %s %s %s" staged-flag ref-arg file-arg)))
		 (shell-command-to-string command)))
   :description "Show git diff. Can show unstaged changes, staged changes, or diff against a ref/branch."
   :args (list '(:name "file-path"
                       :type string
                       :description "Optional specific file to diff (relative to project root)"
                       :optional t)
               '(:name "staged"
                       :type boolean
                       :description "Show staged changes instead of unstaged"
                       :optional t)
               '(:name "ref"
                       :type string
                       :description "Git ref to diff against (e.g., 'main', 'HEAD~3', 'branch1..branch2')"
                       :optional t))
   :category "git")

  ;; History
  (gptel-make-tool
   :name "git_log"
   :function (lambda (&optional max-count file-path)
               (let* ((default-directory (projectile-project-root))
                      (count-arg (if max-count (format "-n %d" max-count) "-n 10"))
                      (file-arg (if file-path (format "-- %s" file-path) ""))
                      (command (format "git log --oneline %s %s" count-arg file-arg)))
                 (shell-command-to-string command)))
   :description "Show git commit history (default 10 commits)"
   :args (list '(:name "max-count"
                       :type integer
                       :description "Maximum number of commits to show"
                       :optional t)
               '(:name "file-path"
                       :type string
                       :description "Show history for specific file only"
                       :optional t))
   :category "git")

  ;; Show specific commit
  (gptel-make-tool
   :name "git_show"
   :function (lambda (commit-hash &optional file-path)
               (let* ((default-directory (projectile-project-root))
                      (file-arg (if file-path (format "-- %s" file-path) ""))
                      (command (format "git show %s %s" commit-hash file-arg)))
                 (shell-command-to-string command)))
   :description "Show details of a specific commit"
   :args (list '(:name "commit-hash"
                       :type string
                       :description "Git commit hash or reference (e.g., 'HEAD', 'main')")
               '(:name "file-path"
                       :type string
                       :description "Show changes for specific file only"
                       :optional t))
   :category "git")

  ;; Remote info
  (gptel-make-tool
   :name "git_remote"
   :function (lambda ()
               (let ((default-directory (projectile-project-root)))
                 (shell-command-to-string "git remote -v")))
   :description "Show git remote repositories"
   :category "git")

  ;; Delete operations
  (gptel-make-tool
   :name "my_delete_file"
   :function (lambda (rel-filepath)
               (let* ((project-root (projectile-project-root))
                      (full-path (expand-file-name rel-filepath project-root)))
                 (if (file-exists-p full-path)
                     (if (file-directory-p full-path)
                         (format "Error: %s is a directory, use my_delete_directory instead" rel-filepath)
                       (delete-file full-path)
                       (format "Deleted file %s" rel-filepath))
                   (format "Error: File %s does not exist" rel-filepath))))
   :description "Delete a file from the current project"
   :args (list '(:name "rel-filepath"
                       :type string
                       :description "Path to the file relative to project root (e.g., 'src/old_file.py')"))
   :category "file-write")

  ;; Git Blame
  (gptel-make-tool
   :name "git_blame"
   :function (lambda (file-path &optional start-line end-line)
               (let* ((default-directory (projectile-project-root))
                      (range-arg (if (and start-line end-line)
                                     (format "-L %d,%d" start-line end-line)
                                   ""))
                      (command (format "git blame %s %s" range-arg file-path)))
		 (shell-command-to-string command)))
   :description "Show who last modified each line of a file, with commit hash and timestamp."
   :args (list '(:name "file-path"
                       :type string
                       :description "Path to the file relative to project root")
               '(:name "start-line"
                       :type integer
                       :description "Start of line range to blame"
                       :optional t)
               '(:name "end-line"
                       :type integer
                       :description "End of line range to blame"
                       :optional t))
   :category "git")

  (gptel-make-tool
   :name "git_file_at_commit"
   :function (lambda (commit-hash file-path)
               (let* ((default-directory (projectile-project-root))
                      (command (format "git show %s:%s" commit-hash file-path)))
		 (shell-command-to-string command)))
   :description "Show the full contents of a file at a specific commit."
   :args (list '(:name "commit-hash"
                       :type string
                       :description "Git commit hash or reference (e.g., 'HEAD', 'main', 'abc1234')")
               '(:name "file-path"
                       :type string
                       :description "Path to the file relative to project root"))
   :category "git")

  (gptel-make-tool
   :name "my_delete_directory"
   :function (lambda (rel-path &optional recursive)
               (let* ((project-root (projectile-project-root))
                      (full-path (expand-file-name rel-path project-root)))
                 (if (file-exists-p full-path)
                     (if (file-directory-p full-path)
                         (if recursive
                             (progn
                               (delete-directory full-path t)
                               (format "Deleted directory %s and all contents" rel-path))
                           (if (directory-files full-path nil "^[^.]")
                               (format "Error: Directory %s is not empty. Use recursive=true to delete with contents" rel-path)
                             (progn
                               (delete-directory full-path)
                               (format "Deleted empty directory %s" rel-path))))
                       (format "Error: %s is not a directory" rel-path))
                   (format "Error: Directory %s does not exist" rel-path))))
   :description "Delete a directory from the current project"
   :args (list '(:name "rel-path"
                       :type string
                       :description "Path to the directory relative to project root (e.g., 'old_folder')")
               '(:name "recursive"
                       :type boolean
                       :description "Whether to delete directory and all its contents"
                       :optional t))
   :category "file-write")

  (gptel-make-tool
   :name "my_patch_file"
   :function (lambda (rel-filepath old-content new-content)
               (let* ((project-root (projectile-project-root))
                      (path (expand-file-name rel-filepath project-root)))
		 (if (not (file-exists-p path))
                     (format "Error: File %s does not exist" rel-filepath)
                   (with-temp-buffer
                     (insert-file-contents path)
                     (let ((matches 0)
                           (search-start (point-min)))
                       (save-excursion
			 (goto-char search-start)
			 (while (search-forward old-content nil t)
                           (setq matches (1+ matches))))
                       (cond
			((= matches 0)
			 (format "Error: Could not find the specified content in %s" rel-filepath))
			((> matches 1)
			 (format "Error: Found %d matches in %s — too ambiguous to patch safely" matches rel-filepath))
			(t
			 (goto-char (point-min))
			 (search-forward old-content)
			 (replace-match new-content t t)
			 (write-file path)
			 (format "Patched %s successfully" rel-filepath))))))))
   :description "Make a targeted edit to an existing file by replacing a specific block of text. Read the file first to ensure you have the exact current content."
   :args (list '(:name "rel-filepath"
                       :type string
                       :description "Path to the file relative to project root (e.g., 'src/main.py')")
               '(:name "old-content"
                       :type string
                       :description "The exact block of text to find and replace. Must match precisely.")
               '(:name "new-content"
                       :type string
                       :description "The new content to replace the old block with."))
   :category "file-write")

  ;; Rename/move operations
  (gptel-make-tool
   :name "my_rename_file"
   :function (lambda (old-rel-path new-rel-path)
               (let* ((project-root (projectile-project-root))
                      (old-full-path (expand-file-name old-rel-path project-root))
                      (new-full-path (expand-file-name new-rel-path project-root)))
                 (cond
                  ((not (file-exists-p old-full-path))
                   (format "Error: Source file %s does not exist" old-rel-path))
                  ((file-exists-p new-full-path)
                   (format "Error: Destination %s already exists" new-rel-path))
                  (t
                   ;; Create destination directory if needed
                   (let ((new-dir (file-name-directory new-full-path)))
                     (when (and new-dir (not (file-exists-p new-dir)))
                       (make-directory new-dir t)))
                   (rename-file old-full-path new-full-path)
                   (format "Renamed %s to %s" old-rel-path new-rel-path)))))
   :description "Rename or move a file within the current project"
   :args (list '(:name "old-rel-path"
                       :type string
                       :description "Current path of the file relative to project root")
               '(:name "new-rel-path"
                       :type string
                       :description "New path for the file relative to project root"))
   :category "file-write")

  (gptel-make-tool
   :name "my_copy_file"
   :function (lambda (source-rel-path dest-rel-path)
               (let* ((project-root (projectile-project-root))
                      (source-full-path (expand-file-name source-rel-path project-root))
                      (dest-full-path (expand-file-name dest-rel-path project-root)))
                 (cond
                  ((not (file-exists-p source-full-path))
                   (format "Error: Source file %s does not exist" source-rel-path))
                  ((file-directory-p source-full-path)
                   (format "Error: %s is a directory, not a file" source-rel-path))
                  ((file-exists-p dest-full-path)
                   (format "Error: Destination %s already exists" dest-rel-path))
                  (t
                   ;; Create destination directory if needed
                   (let ((dest-dir (file-name-directory dest-full-path)))
                     (when (and dest-dir (not (file-exists-p dest-dir)))
                       (make-directory dest-dir t)))
                   (copy-file source-full-path dest-full-path)
                   (format "Copied %s to %s" source-rel-path dest-rel-path)))))
   :description "Copy a file within the current project"
   :args (list '(:name "source-rel-path"
                       :type string
                       :description "Path of the source file relative to project root")
               '(:name "dest-rel-path"
                       :type string
                       :description "Path for the destination file relative to project root"))
   :category "file-write"))

(defun my/gptel-aws-sso-login (profile)
  "Login to AWS SSO and set PROFILE for gptel-bedrock."
  (interactive (list (completing-read "AWS Profile: " 
                                      (split-string (shell-command-to-string 
                                                     "aws configure list-profiles") "\n" t))))
  (message "Logging in to AWS SSO with profile %s..." profile)
  (if (zerop (call-process "aws" nil nil nil "sso" "login" "--profile" profile))
      (progn
        (setenv "AWS_PROFILE" profile)
        (message "AWS SSO login successful, set profile to %s for gptel-bedrock" profile))
    (message "AWS SSO login failed for profile %s" profile)))

(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install))

(use-package kubernetes)

;; Configure warnings
(setq warning-minimum-level :error)

(require 'gnutls)
(setq gnutls-trustfiles
      (append gnutls-trustfiles
              '("/Users/james.hood-smith/zscaler-root-ca.pem")))

;;; init.el ends here
