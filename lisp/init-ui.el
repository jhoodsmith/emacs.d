;;; init-ui.el --- Look and feel  -*- lexical-binding: t -*-

;;; Commentary:

;; Theme, icons, window management and visual feedback.

;;; Code:

;; The tool bar and scroll bars are disabled in early-init.el.

(use-package dracula-theme
  :config (load-theme 'dracula t))

(use-package all-the-icons
  :if (display-graphic-p))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package rainbow-delimiters
  :hook prog-mode)

;; Never lose the cursor.
(use-package beacon
  :diminish
  :init
  (add-hook 'after-init-hook 'beacon-mode))

;; Show available keybindings.
(use-package which-key
  :diminish
  :init
  (add-hook 'after-init-hook 'which-key-mode))

;; Visual bell: flash the mode line.
(defun my/flash-mode-line ()
  "Visual bell."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq visible-bell nil
      ring-bell-function #'my/flash-mode-line)

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

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-all-the-icons
  :after treemacs
  :config
  (treemacs-load-theme "all-the-icons"))

(provide 'init-ui)
;;; init-ui.el ends here
