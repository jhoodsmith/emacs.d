;;; early-init.el --- Runs before the first frame is drawn  -*- lexical-binding: t -*-

;;; Commentary:

;; Anything that must happen before the GUI frame is created or packages are
;; initialised belongs here.

;;; Code:

(setq native-comp-jit-compilation nil)

;; Avoid garbage collection while loading the config, then restore a sensible
;; threshold once startup has finished.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024))))

;; Keyboard-centric UI: no tool bar or scroll bars (set here so they never
;; flash on screen).
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;; early-init.el ends here
