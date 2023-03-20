;;;; init.el -- My Emacs configuration
;;;; Commentary:

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
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 truncate-lines nil
 truncate-partial-width-windows nil)

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
              ("M-i" . symbol-overlay-put)
              ("M-I" . symbol-overlay-remove-all)
              ("M-n" . symbol-overlay-jump-next)
              ("M-i" . symbol-overlay-jump-prev)))

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

;; Documentation popup
(use-package company-quickhelp
  :init
  (add-hook 'after-init-hook 'company-quickhelp-mode))

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

;; Moving and duplicating lines of rectangles
(use-package move-dup
  :bind (("M-p"   . move-dup-move-lines-up)
         ("C-M-p" . move-dup-duplicate-up)
         ("M-n"   . move-dup-move-lines-down)
         ("C-M-n" . move-dup-duplicate-down))
  :init
  (add-hook 'after-init-hook 'move-dup-mode))

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
  :custom
  (completion-styles '(substring orderless)))

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

(provide 'init)
;;; init.el ends here
