;;; init-lang-python.el --- Python  -*- lexical-binding: t -*-

;;; Commentary:

;; Python via pylsp run through uv.

;;; Code:

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-ts-mode python-mode) . ("uv" "run" "pylsp"))))

(use-package pytest)

;; The built-in python.el (not the MELPA `python-mode'), which owns
;; `python-ts-mode-map'.
(use-package python
  :ensure nil
  :bind (:map python-ts-mode-map
              ("C-c C-t t" . pytest-one)))

;; `python-mode' is remapped to `python-ts-mode' (see init-lsp.el), which does
;; not run `python-mode-hook', so hook the shared base mode instead.
(use-package pyvenv
  :config
  (add-hook 'python-base-mode-hook #'pyvenv-mode)
  (add-hook 'python-base-mode-hook #'pyvenv-tracking-mode))

(defun my/uv-activate ()
  "Activate the uv venv for the current project."
  (interactive)
  (let ((venv (concat (projectile-project-root) ".venv")))
    (when (file-exists-p venv)
      (pyvenv-activate venv))))

(provide 'init-lang-python)
;;; init-lang-python.el ends here
