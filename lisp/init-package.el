;;; init-package.el --- Package archives and use-package  -*- lexical-binding: t -*-

;;; Commentary:

;; Must be loaded before any other module: it sets up MELPA and use-package.

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;; Uncomment to profile startup, then run M-x use-package-report.
;; (setq use-package-compute-statistics t)

;; Needed by the :diminish keyword, so install it before anything else.
(use-package diminish)

(provide 'init-package)
;;; init-package.el ends here
