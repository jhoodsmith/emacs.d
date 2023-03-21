;;; init.el -- My Emacs configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2023  James Hood-Smith
;; Author: James Hood-Smith <james@hood-smith.co.uk>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

;; My local lisp functions
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq elisp-flymake-byte-compile-load-path load-path)

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure 't)

;; Global keybindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-3") (lambda () (interactive) (insert "#")))

;; Personal configuration
(setq-default
 user-full-name "James Hood-Smith"
 user-mail-address "james@hood-smith.me.uk"
 ispell-dictionary "british")

;; Some basic preferences
(setq-default
 fill-column 80
 buffers-menu-max-size 30
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
(setq-default display-line-numbers-width 3)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook prog-mode)

;; Symbol overlay
(use-package symbol-overlay
  :diminish
  :hook prog-mode
  :bind (:map symbol-overlay-mode-map
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev)))

;; Visual bell
(defun flash-mode-line ()
  "Flash mode string."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq visible-bell nil ring-bell-function 'flash-mode-line)

;; Keyboard-centric user interface
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
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

(use-package git-timemachine)

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

;; Vertical display in minibuffer
(use-package vertico
  :init
  (add-hook 'after-init-hook 'vertico-mode))

;; orderless completion style
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Do not use Orderless with Company
(defun company-completion-styles (capf-fn &rest args)
  "Set Company-specific completion-styles.

   CAPF-FN is `company-capf' function.
   ARGS is list of function arguments."
  (let ((completion-styles '(basic partial-completion)))
    (apply capf-fn args)))
(advice-add 'company-capf :around #'company-completion-styles)

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

;; Ruby
(use-package inf-ruby)

;; Org
(define-key global-map (kbd "C-c c")
            (lambda () (interactive) (org-capture nil "n")))
(setq-default org-confirm-babel-evaluate nil
              org-image-actual-width nil
              org-src-fontify-natively t
              org-agenda-files '("~/org")
              org-capture-templates '(("n" "Note" entry (file "~/org/inbox.org")
                                       "* %?\nEntered on %U\n  %i\n  %a")))

;; Enable languages for Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (sql . t)
   (R . t)
   (haskell . t)
   (clojure . t)
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
  :mode (("\\.rest\\'" . restclient-mode)
         ("\\.http\\'" . restclient-mode)))

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
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "LANG"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; Switch Java
(require 'switch-java)

;; eglot
(with-eval-after-load 'eglot
  (setq-default eglot-workspace-configuration
                '(:solargraph (:diagnostics t)))
  (add-to-list 'eglot-server-programs
               '(ruby-mode . ("localhost" 7658))))

(provide 'init)
;;; init.el ends here
