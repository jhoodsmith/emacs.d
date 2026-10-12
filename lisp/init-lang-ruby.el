;;; init-lang-ruby.el --- Ruby and Rails  -*- lexical-binding: t -*-

;;; Commentary:

;; Ruby via Solargraph (run through Bundler), plus Rails and RSpec helpers.

;;; Code:

(with-eval-after-load 'eglot
  ;; To use a Solargraph server running elsewhere instead:
  ;; (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) . ("localhost" 7658)))
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) . ("bundle" "exec" "solargraph" "stdio"))))

(defun my/ruby-set-lsp-config ()
  "Load LSP config from solargraph.json."
  (setq eglot-workspace-configuration
        (let ((config-file (file-name-concat user-emacs-directory "lsp-config" "solargraph.json")))
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
    (inf-ruby-console-run "bundle exec rails c -- --nomultiline" "rails"))

  (defun my/bundler-console ()
    "Run a Bundler console (bin/console)."
    (interactive)
    (inf-ruby-console-run "bin/console" "ruby")))

(use-package ruby-mode
  :ensure nil
  :bind (:map ruby-mode-map
              ("C-c C-c" . ruby-send-buffer)))

(use-package projectile-rails
  :bind-keymap
  ("C-c r" . projectile-rails-command-map)
  :config
  (projectile-rails-global-mode))

(use-package slim-mode)
(use-package rspec-mode)
(use-package rubocop)

(provide 'init-lang-ruby)
;;; init-lang-ruby.el ends here
