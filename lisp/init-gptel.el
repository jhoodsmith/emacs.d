;;; init-gptel.el --- gptel LLM client: backends and presets  -*- lexical-binding: t -*-

;;; Commentary:

;; Backends, presets and settings for gptel.  The tools it can call are in
;; init-gptel-tools.el.  API keys are fetched lazily (when a request is made)
;; so that starting Emacs never prompts for the GPG passphrase.

;;; Code:

;; Provided by local.el.
(defvar my/gptel-load-path)
(defvar my/gptel-cv-file)

;; Prefer a local checkout of gptel when one is configured.
(when (and (bound-and-true-p my/gptel-load-path)
           (file-directory-p my/gptel-load-path))
  (add-to-list 'load-path my/gptel-load-path))

(defun my/auth-key (host)
  "Return a function that looks up the password for HOST when called."
  (lambda () (auth-source-pick-first-password :host host)))

(use-package gptel
  :bind (("C-c g s" . gptel-send)
         ("C-c g a" . gptel-add)
         ("C-c g m" . gptel-menu)
         ("C-c g g" . gptel)
         ("C-c g f" . gptel-add-file)
         ("C-c g w" . gptel-rewrite)
         ("C-c g r" . gptel-context-remove-all))
  :config
  (require 'gptel-integrations)
  (require 'gptel-bedrock)
  (push '(eu-claude-haiku-5.5-profile . "eu.anthropic.claude-haiku-5-5") gptel-bedrock--model-ids)
  (push '(eu-claude-sonnet-4.6-profile . "eu.anthropic.claude-sonnet-4-6") gptel-bedrock--model-ids)
  (push '(eu-claude-sonnet-5.5-profile . "eu.anthropic.claude-sonnet-5-5") gptel-bedrock--model-ids)

  (defvar gptel-backend-bedrock
    (gptel-make-bedrock "Bedrock"
      :stream t
      :models '(eu-claude-haiku-5.5-profile eu-claude-sonnet-4.6-profile eu-claude-sonnet-5.5-profile)
      :region "eu-west-2"))

  (defvar gptel-backend-gemini
    (gptel-make-gemini "Gemini"
      :key (my/auth-key "generativelanguage.googleapis.com")))

  (gptel-make-preset 'gemini-with-search
    :description "A preset for Gemini with web search"
    :backend "Gemini"
    :include-reasoning nil
    :model 'gemini-2.5-flash
    :system "You are a helpful assistant working within Emacs. When presenting results from web search always include the URL for each result in your response so I can verify information."
    :request-params '(:tools [(:google_search ()) (:url_context ())]))

  (defvar gptel-backend-gh
    (gptel-make-gh-copilot "Copilot"))

  (defvar gptel-backend-anthropic
    (gptel-make-anthropic "Claude"
      :stream t
      :key (my/auth-key "api.anthropic.com")))

  (gptel-make-preset 'claude-with-search
    :description "A preset for Claude with web search"
    :backend "Claude"
    :model 'claude-sonnet-5
    :context (when (bound-and-true-p my/gptel-cv-file)
               (list my/gptel-cv-file))
    :system "You are a helpful assistant working within Emacs. When presenting results from web search always give me URL so I can cross check."
    :request-params '(:tools [(:type "web_search_20260209" :name "web_search" :allowed_callers ["direct"])
                              (:type "web_fetch_20260209" :name "web_fetch" :allowed_callers ["direct"])]))

  (gptel-make-preset 'bedrock-with-search
    :description "A preset for Bedrock with Claude 4.6 and web search"
    :backend "Bedrock"
    :model 'eu-claude-sonnet-4.6-profile
    :context (when (bound-and-true-p my/gptel-cv-file)
               (list my/gptel-cv-file))
    :system "You are a helpful assistant working within Emacs. When presenting results from web search always give me URL so I can cross check. I live near London, England."
    :tools '("my_web_search" "my_fetch_webpage"))

  (defvar gptel-backend-openai
    (gptel-make-openai "ChatGPT"
      :stream t
      :models gptel--openai-models
      :key (my/auth-key "api.openai.com")))

  (setf (alist-get 'default gptel-directives)
        "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")

  (setq gptel-default-mode 'org-mode
        gptel-log-level 'info
        gptel-model 'claude-3.7-sonnet
        gptel-cache t
        gptel-max-tokens 8192
        ;; gptel-model 'gemini-2.5-flash
        gptel-backend gptel-backend-gh)

  ;; Tools the model may call.
  (require 'init-gptel-tools))

(defun my/gptel-aws-sso-login (profile)
  "Login to AWS SSO and set PROFILE for gptel-bedrock."
  (interactive (list (completing-read "AWS Profile: "
                                      (process-lines "aws" "configure" "list-profiles"))))
  (message "Logging in to AWS SSO with profile %s..." profile)
  (if (zerop (call-process "aws" nil nil nil "sso" "login" "--profile" profile))
      (progn
        (setenv "AWS_PROFILE" profile)
        (message "AWS SSO login successful, set profile to %s for gptel-bedrock" profile))
    (message "AWS SSO login failed for profile %s" profile)))

(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install))

(provide 'init-gptel)
;;; init-gptel.el ends here
