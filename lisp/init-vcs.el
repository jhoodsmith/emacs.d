;;; init-vcs.el --- Version control and project management  -*- lexical-binding: t -*-

;;; Commentary:

;; Magit and Projectile.

;;; Code:

(use-package magit
  :bind (("C-x g" . magit-status))
  :custom
  (magit-repository-directories
   '(;; Directory and depth to search
     ("~/work/"     . 1)
     ("~/org/"      . 0)
     ("~/.emacs.d/" . 0))))

(use-package git-timemachine
  :bind ("C-x v t" . git-timemachine-toggle))

(defun my/projectile-register-magit-repos ()
  "Tell Projectile about every repository Magit knows of."
  (require 'magit)
  (mapc #'projectile-add-known-project
        (mapcar #'file-name-as-directory (magit-list-repos))))

(use-package projectile
  :diminish
  :hook (after-init . projectile-mode)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  ;; Scanning every repository is slow, so do it once Emacs is idle rather
  ;; than during startup.
  (run-with-idle-timer 2 nil #'my/projectile-register-magit-repos))

(provide 'init-vcs)
;;; init-vcs.el ends here
