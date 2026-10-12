;;; init-lang-clojure.el --- Clojure and Lisp editing  -*- lexical-binding: t -*-

;;; Commentary:

;; Clojure tooling, plus Paredit which is shared with Emacs Lisp.

;;; Code:

(use-package clojure-mode)
(use-package cider)

;; Install deps-new:
;;   clojure -Ttools install-latest :lib io.github.seancorfield/deps-new :as new
;; Check that it's installed:
;;   clojure -Ttools list
(use-package clj-deps-new)

(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
         (clojure-mode . paredit-mode)
         (clojurescript-mode . paredit-mode)
         (cider-repl-mode . paredit-mode))
  :bind (:map paredit-mode-map ("RET" . nil)))

(provide 'init-lang-clojure)
;;; init-lang-clojure.el ends here
