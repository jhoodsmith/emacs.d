;;; init-completion.el --- Minibuffer and in-buffer completion  -*- lexical-binding: t -*-

;;; Commentary:

;; Vertico + Orderless + Consult in the minibuffer, Company in buffers.

;;; Code:

;; Vertical display in the minibuffer
(use-package vertico
  :init
  (add-hook 'after-init-hook 'vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Search and navigation
(use-package consult
  :bind
  (([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap goto-line] . consult-goto-line))
  :custom
  ;; Use Projectile's idea of the project root (replaces the obsolete
  ;; `consult-project-root-function').
  (consult-project-function
   (lambda (_may-prompt)
     (when (fboundp 'projectile-project-root)
       (projectile-project-root)))))

;; Company
(use-package company
  :diminish company-mode
  :init
  (setq tab-always-indent 'complete)
  :bind (([remap completion-at-point] . company-complete)
         ([remap indent-for-tab-command] . company-indent-or-complete-common)
         :map company-active-map
         ;; Navigate the completion popup with C-n and C-p.
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (global-company-mode t))

(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode))

;; Do not use Orderless with Company.
(defun my/company-completion-styles (capf-fn &rest args)
  "Call CAPF-FN with ARGS using basic completion styles instead of Orderless."
  (let ((completion-styles '(basic partial-completion)))
    (apply capf-fn args)))
(advice-add 'company-capf :around #'my/company-completion-styles)

(provide 'init-completion)
;;; init-completion.el ends here
