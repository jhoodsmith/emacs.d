;;; init-lang-rust.el --- Rust  -*- lexical-binding: t -*-

;;; Commentary:

;; brew install rust rust-analyzer
;; cargo new hello_world --bin

;;; Code:

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((rust-mode rust-ts-mode) . ("rust-analyzer" :initializationOptions
                                             ( :procMacro (:enable t)
                                               :cargo ( :buildScripts (:enable t)
                                                        :features "all"))))))

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

(provide 'init-lang-rust)
;;; init-lang-rust.el ends here
