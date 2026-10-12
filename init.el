;;; init.el --- My Emacs configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2026 James Hood-Smith
;; Author: James Hood-Smith <james@hood-smith.co.uk>
;; URL: https://github.com/jhoodsmith/emacs.d

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This is my personal Emacs configuration.  This file only bootstraps; the
;; real work lives in lisp/init-*.el.  Machine- and employer-specific values
;; (email, hosts, local paths, tokens) live in local.el, which is not
;; committed.  See local.el.example.

;;; Code:

(let ((minver "29"))
  (when (version< emacs-version minver)
    (error "You're running Emacs %s.  This config requires version %s or higher"
           emacs-version minver)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq elisp-flymake-byte-compile-load-path load-path)

;; Package archives and use-package must come first.
(require 'init-package)

;; Machine-specific settings, then Customize output.
(load (expand-file-name "local.el" user-emacs-directory) 'noerror)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; General
(require 'init-core)
(require 'init-ui)
(require 'init-completion)
(require 'init-vcs)

;; Writing
(require 'init-org)

;; Programming
(require 'init-lsp)
(require 'init-lang-ruby)
(require 'init-lang-python)
(require 'init-lang-go)
(require 'init-lang-rust)
(require 'init-lang-web)
(require 'init-lang-clojure)
(require 'init-lang-infra)
(require 'init-lang-misc)

;; AI
(require 'init-gptel)
(require 'init-mcp)

;;; init.el ends here
