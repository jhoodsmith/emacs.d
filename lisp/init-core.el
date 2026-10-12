;;; init-core.el --- Basic preferences and general-purpose tools  -*- lexical-binding: t -*-

;;; Commentary:

;; Editor behaviour that isn't tied to a language, theme or workflow.

;;; Code:

(require 'subr-x)

;; Values below are normally provided by local.el (loaded before this file).
;; `defvar' without a value leaves any value already set there intact.
(defvar my/extra-trustfiles)

;;;; Personal

(setq user-full-name "James Hood-Smith"
      ispell-dictionary "british")
;; `user-mail-address' is set in local.el.

;;;; Basic preferences

;; These variables are buffer-local when set, so they need `setq-default' to
;; have any effect outside the current buffer.
(setq-default fill-column 80
              case-fold-search t
              indent-tabs-mode nil
              truncate-lines nil)

(setq create-lockfiles nil
      auto-save-default nil
      make-backup-files nil
      use-short-answers t
      inhibit-startup-message t)

(column-number-mode 1)

;; Warnings: hide byte/native-compiler noise but keep real problems visible.
(setq warning-suppress-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)

;;;; Keybindings

(when (eq system-type 'darwin)
  ;; UK keyboard: M-3 produces a hash sign.
  (global-set-key (kbd "M-3") (lambda () (interactive) (insert "#"))))

(defun my/delete-whitespace-forward ()
  "Delete whitespace forward up to the next non-whitespace character."
  (interactive)
  (delete-region (point)
                 (progn (skip-chars-forward " \t\n\r") (point))))

(global-set-key (kbd "C-c d") #'my/delete-whitespace-forward)

;;;; Server

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

;;;; Environment

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-shell-name "/bin/zsh")
  (setq exec-path-from-shell-variables
        (append exec-path-from-shell-variables
                '("NODE_EXTRA_CA_CERTS" "DIGITALOCEAN_ACCESS_TOKEN" "AWS_CA_BUNDLE")))
  (exec-path-from-shell-initialize))

;; Extra CA bundles (e.g. a corporate proxy), configured in local.el.
(when (bound-and-true-p my/extra-trustfiles)
  (require 'gnutls)
  (setq gnutls-trustfiles (append gnutls-trustfiles my/extra-trustfiles)))

;;;; Editing helpers

(use-package elisp-demos
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))

;; Moving and duplicating lines or rectangles
(use-package move-dup
  :bind (([M-up] . move-dup-move-lines-up)
         ([M-down] . move-dup-move-lines-down))
  :init
  (add-hook 'after-init-hook 'move-dup-mode))

;; Edit regions in separate buffers
(use-package edit-indirect)

(use-package yasnippet
  :bind (("C-c y s" . yas-insert-snippet)
         ("C-c y v" . yas-visit-snippet-file))
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;; Formatters on save; used by the language modules.
(use-package reformatter)

;;;; Spelling

;; brew install aspell
(use-package flyspell
  :ensure nil
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package osx-dictionary)

;;;; Search

;; brew install ripgrep
(use-package rg
  :config
  (rg-enable-default-bindings))

;;;; Text formats

(use-package markdown-mode)

;;;; Terminal and browser

(use-package vterm
  :custom (vterm-install t))

(use-package eww
  :ensure nil
  :commands (eww eww-follow-link)
  :init
  (setq eww-search-prefix "https://www.google.com/search?q=")

  (defun eww-wiki (text)
    "Search Wikipedia for TEXT."
    (interactive (list (read-string "Wiki for: ")))
    (eww (format "https://en.m.wikipedia.org/wiki/Special:Search?search=%s"
                 (url-encode-url text))))

  :bind (("C-c w w" . eww)
         ("C-c w i" . eww-wiki)
         ("C-c w l" . eww-follow-link)))

;;;; Secrets

;; Credentials are read with `auth-source-pick-first-password'.  Setup:
;;   brew install gnupg
;;   Write entries to ~/.authinfo, then: gpg -c ~/.authinfo; rm ~/.authinfo
;;   Open the .gpg file in Emacs and give the passphrase.
;;   Ensure ~/.gnupg/gpg.conf contains:
;;     use-agent
;;     pinentry-mode loopback
;;   Ensure ~/.gnupg/gpg-agent.conf contains:
;;     allow-loopback-pinentry
;;   Decrypt from the command line with: gpg -d ~/.authinfo.gpg

(provide 'init-core)
;;; init-core.el ends here
