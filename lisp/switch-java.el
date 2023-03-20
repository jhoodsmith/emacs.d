;;; switch-java.el --- Switching versions of Java on Mac -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar switch-java-java-base
  "/Library/Java/JavaVirtualMachines"
  "Base Java directory.")

(defvar switch-java--path)
(defvar switch-java--java-path)

(defun switch-java-all-versions ()
  "Return the list of installed JDKs."
  (seq-remove
   (lambda (x) (or (equal x ".") (equal x "..")))
   (directory-files switch-java-java-base)))

(defun switch-java--save-env ()
  "Store original PATH and JAVA_HOME."
  (when (not (boundp 'switch-java--java-path))
    (setq switch-java--java-path (getenv "JAVA_PATH")))
  (when (not (boundp 'switch-java--path))
             (setq switch-java--path (getenv "PATH"))))

(defun switch-java-show-version ()
  "Display the currently selected Java version."
  (interactive)
  (message (concat "Java HOME: " (getenv "JAVA_HOME"))))

(defun switch-java-select ()
  "List the installed JDKs and switch to the version that is selected."
  (interactive)
  (switch-java--save-env)

  (let* ((new-java-home (file-name-concat switch-java-java-base
                                          (completing-read
                                           "Which Java: "
                                           (switch-java-all-versions))
                                          "Contents"
                                          "Java"))
         (new-java-path (file-name-concat new-java-home "bin" "java")))
    (setenv "JAVA_HOME" new-java-home)
    (setenv "PATH" (concat new-java-path ":" switch-java--path))
    (switch-java-show-version)))

(provide 'switch-java)
;;; switch-java.el ends here
