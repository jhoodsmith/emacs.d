;;; init-lang-infra.el --- Infrastructure, containers and toolchain managers  -*- lexical-binding: t -*-

;;; Commentary:

;; Terraform, Docker, Kubernetes/Helm, and the version managers that put the
;; right tools on PATH.

;;; Code:

;; Toolchain managers (see lisp/asdf.el and lisp/switch-java.el)
(require 'switch-java)

(require 'asdf)
(asdf-enable)
(setq asdf-binary "/opt/homebrew/opt/asdf/bin/asdf")

;; Terraform
(use-package terraform-mode)
(use-package company-terraform
  :hook (terraform-mode . company-terraform-init))

;; Docker
(use-package dockerfile-mode)
(use-package docker-compose-mode)

;; Kubernetes
(use-package kubernetes)

;; Helm chart templates.  Named `helm-chart-mode' so that it doesn't clash
;; with the Helm completion framework's `helm-mode'.
(use-package yaml-mode)

(define-derived-mode helm-chart-mode yaml-mode "helm"
  "Major mode for editing Kubernetes Helm templates.")

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(helm-chart-mode "helm_ls" "serve")))

(provide 'init-lang-infra)
;;; init-lang-infra.el ends here
