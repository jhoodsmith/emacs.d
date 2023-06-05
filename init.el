;;; init.el -- My Emacs configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2023 James Hood-Smith
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
(global-set-key (kbd "M-3") (lambda () (interactive) (insert "#")))

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

;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Line numbering
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

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

;; Visual bell
(defun my/flash-mode-line ()
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
(use-package exotica-theme
  :config (load-theme 'exotica t))

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

;; spelling
(require 'ispell)
(when (executable-find ispell-program-name)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;; markdown
(use-package markdown-mode)

;; Edit regions in separate buffers
(use-package edit-indirect)

;; Org
(defun my/new-org-note ()
     (interactive)
     (org-capture nil "n"))
(global-set-key (kbd "C-c n") #'my/new-org-note)

(setq-default org-confirm-babel-evaluate nil
              org-image-actual-width nil
              org-startup-with-inline-images t
              org-src-window-setup 'other-window
              org-src-fontify-natively t
              org-agenda-files (directory-files-recursively "~/org" "\\.org$")
              org-capture-templates '(("n" "Note" entry (file "~/org/inbox.org")
                                       "* %?\nEntered on %U\n  %i\n  %a")))

;; update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

(use-package ob-restclient)

;; Enable languages for Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (sql . t)
   (R . t)
   (dot . t)
   (haskell . t)
   (clojure . t)
   (restclient . t)
   (java . t)
   (plantuml . t)
   (python . t)))

;; Plantuml
(use-package plantuml-mode
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
  (org-plantuml-jar-path (expand-file-name "~/bin/plantuml.jar")))

;; Export to Hugo
(use-package ox-hugo)

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

;; Exec path from shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; Switch Java
(require 'switch-java)

;; Eglot
(use-package eglot
  :ensure nil
  :config
  (setq eglot-autoshutdown t
        eglot-stay-out-of '(eldoc)))

(use-package consult-eglot)

(with-eval-after-load 'eglot
  ;; (add-to-list 'eglot-server-programs
  ;;              '(ruby-mode . ("localhost" 7658))))
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) . ("bundle" "exec" "solargraph" "stdio"))))

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
    (inf-ruby-console-run "bundle exec rails c -- --nomultiline" "rails")))

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
;; clojure -Ttools install io.github.seancorfield/deps-new '{:git/tag "v0.5.1"}' :as new
(use-package clj-deps-new)

(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
         (clojure-mode . paredit-mode)
         (clojurescript-mode . paredit-mode)
         (cider-repl-mode . paredit-mode))
  :bind (:map paredit-mode-map ("RET" . nil)))
(provide 'init)

;; Emacs package for working with Graphviz DOT-format files
(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

;; Dictionary configuration
(use-package dictionary
  :bind (("C-c l" . dictionary-lookup-definition))
  :custom (dictionary-server "dict.org"))

;; Node
(use-package nodejs-repl
  :bind (:map js-mode-map
         ("C-x C-e" . nodejs-repl-send-last-expression)
         ("C-c C-r" . nodejs-repl-send-region)))

;;  Tree sitter
(use-package treesit
  :ensure nil
  :custom
  (treesit-extra-load-path
   `(,(expand-file-name "elpa/tree-sitter-module/dist/" user-emacs-directory)))
  (treesit-font-lock-level 4)
  :init
  (push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
  (push '(js-mode . js-ts-mode) major-mode-remap-alist)
  (push '(java-mode . java-ts-mode) major-mode-remap-alist)
  (push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
  (push '(ruby-mode . ruby-ts-mode) major-mode-remap-alist)
  (push '(python-mode . python-ts-mode) major-mode-remap-alist))

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

;; Typescript
(use-package typescript-ts-mode
  :hook
  (tsx-ts-mode . sgml-electric-tag-pair-mode)
  :mode
  ((rx ".ts" eos) . typescript-ts-mode)
  ((rx ".tsx" eos) . tsx-ts-mode))

;; Copilot
;; Not convinced that this works for me yet.
(use-package editorconfig)

(use-package copilot
  :load-path (lambda () (expand-file-name "copilot.el" user-emacs-directory))
  :diminish
  :init)

;; (global-copilot-mode)

;; Org-AI
(use-package org-ai
  :commands (org-ai-mode org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  (org-ai-global-mode)

  :config
  (org-ai-install-yasnippets))

;; Configure warnings
(setq-default warning-minimum-level :error)

;;; init.el ends here
