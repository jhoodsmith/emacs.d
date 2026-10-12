;;; init-org.el --- Org mode, Babel and publishing  -*- lexical-binding: t -*-

;;; Commentary:

;; Org itself, its appearance, Babel language support and export/slide tools.

;;; Code:

(defun my/new-org-note ()
  "Create a new note in ~/org/learnings/learnings.org."
  (interactive)
  (org-capture nil "n"))

(global-set-key (kbd "C-c n") #'my/new-org-note)

(setq org-confirm-babel-evaluate nil
      org-image-actual-width nil
      org-startup-with-inline-images t
      org-src-window-setup 'other-window
      org-tags-column -88
      org-hide-emphasis-markers t
      org-src-fontify-natively t
      org-babel-python-command "python3"
      org-capture-templates '(("n" "Note" entry (file "~/org/learnings/learnings.org")
                               "* %?%^g\nEntered on %U\n  %i\n")))

(with-eval-after-load 'org
  ;; Evaluated lazily, and only if ~/org exists.
  (when (file-directory-p "~/org")
    (setq org-agenda-files (directory-files-recursively "~/org" "\\.org\\'")))

  ;; brew install ditaa; the jar name carries the version, so look it up.
  (setq org-ditaa-jar-path
        (or (car (file-expand-wildcards
                  "/opt/homebrew/opt/ditaa/libexec/ditaa-*-standalone.jar"))
            "/opt/homebrew/opt/ditaa/libexec/ditaa-0.11.0-standalone.jar"))

  ;; Update images in the buffer after I evaluate.
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images)

  ;; Enable languages for Babel.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (awk . t)
     (clojure . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (go . t)
     (haskell . t)
     (java . t)
     (latex . t)
     (mermaid . t)
     (plantuml . t)
     (python . t)
     (restclient . t)
     (ruby . t)
     (shell . t)
     (typescript . t)
     (sql . t))))

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("typescript" . typescript-ts)))

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :custom
  (org-modern-star '("◉" "○" "●" "○" "●" "○" "●"))
  (org-modern-list '((43 . "➤") (45 . "–") (42 . "•")))
  (org-modern-tag nil)
  (org-modern-priority nil)
  (org-modern-todo nil)
  (org-modern-table t)
  (org-modern-block-fringe nil)
  (org-modern-block-name nil)
  (org-modern-horizontal-rule nil)
  (org-modern-hide-stars nil))

;;;; Babel language packages

(use-package ob-restclient)
(use-package ob-typescript)
(use-package ob-go)
(use-package gnuplot)
(use-package mermaid-mode)

;; Requires mermaid-cli: npm install -g @mermaid-js/mermaid-cli
(use-package ob-mermaid
  :custom
  (ob-mermaid-cli-path "~/.asdf/shims/mmdc"))

(use-package plantuml-mode
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
  (org-plantuml-jar-path (expand-file-name "~/bin/plantuml.jar")))

;; Converting Jupyter notebooks
(use-package code-cells
  :custom
  (code-cells-convert-ipynb-style
   '(("pandoc" "--to" "ipynb" "--from" "org")
     ("pandoc" "--to" "org" "--from" "ipynb" "--extract-media" "./ipynb-images/")
     (lambda () #'org-mode))))

;;;; Presentations and export

(use-package org-tree-slide
  :bind (:map org-mode-map
              ("s-p" . org-tree-slide-mode)
              ("s-<return>" . org-insert-item)
              :map org-tree-slide-mode-map
              ("<left>" . org-tree-slide-move-previous-tree)
              ("<right>" . org-tree-slide-move-next-tree))
  :hook ((org-tree-slide-play . (lambda ()
                                  (text-scale-increase 2)
                                  (org-display-inline-images)
                                  (read-only-mode 1)))
         (org-tree-slide-stop . (lambda ()
                                  (text-scale-increase 0)
                                  (org-remove-inline-images)
                                  (read-only-mode -1))))
  :custom
  (org-tree-slide-header nil))

;; Export to Hugo
(use-package ox-hugo)

;; Copy selected regions in org-mode as formatted output: ox-clip-formatted-copy
(use-package ox-clip)

(provide 'init-org)
;;; init-org.el ends here
