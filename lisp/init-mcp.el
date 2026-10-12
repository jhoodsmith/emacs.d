;;; init-mcp.el --- Model Context Protocol servers  -*- lexical-binding: t -*-

;;; Commentary:

;; The server list is built when mcp-hub is first loaded rather than at
;; startup, so credentials are only read from auth-source when needed.
;; Machine-specific values come from local.el.

;;; Code:

;; Provided by local.el.
(defvar my/atlassian-host)
(defvar my/jupyter-token)

(defun my/auth-password (host)
  "Return the auth-source password stored for HOST."
  (auth-source-pick-first-password :host host))

(defun my/auth-field (host field)
  "Return FIELD (a keyword such as :user) of the first auth-source entry for HOST."
  (plist-get (car (auth-source-search :host host)) field))

(defun my/mcp-servers ()
  "Build the value of `mcp-hub-servers', reading credentials from auth-source."
  `(("github"
     :command "github-mcp-server"
     :args ("stdio")
     :env (:GITHUB_PERSONAL_ACCESS_TOKEN ,(my/auth-password "api.github.com")))
    ("figma"
     :command "npx"
     :args ("-y" "figma-developer-mcp" "--stdio"
            ,(concat "--figma-api-key=" (my/auth-password "api.figma.com"))))
    ("aws-documentation-mcp-server"
     :command "uvx"
     :args ("awslabs.aws-documentation-mcp-server@latest")
     :env (:FASTMCP_LOG_LEVEL "ERROR" :AWS_DOCUMENTATION_PARTITION "aws"))
    ,@(when (bound-and-true-p my/atlassian-host)
        `(("mcp-atlassian"
           :command "uvx"
           :args ("mcp-atlassian")
           :env (:JIRA_URL ,(concat "https://" my/atlassian-host)
                 :JIRA_USERNAME ,(my/auth-field my/atlassian-host :user)
                 :JIRA_API_TOKEN ,(my/auth-password my/atlassian-host)))))
    ("gocardless" :url "https://mcp.gocardless.com")
    ("agent-toolkit-for-aws" :url "http://localhost:8765/mcp")
    ("ruby-api-eval" :url "http://localhost:3000/mcp")
    ("google-workspace"
     :command "uvx"
     :args ("workspace-mcp" "--tool-tier" "core")
     :env (:GOOGLE_OAUTH_CLIENT_SECRET ,(my/auth-password "oauth2.googleapis.com")
           ;; The client ID is kept as the login of the same auth-source entry.
           :GOOGLE_OAUTH_CLIENT_ID ,(my/auth-field "oauth2.googleapis.com" :user)
           ;; workspace-mcp's OAuth callback is plain http on localhost.
           :OAUTHLIB_INSECURE_TRANSPORT "1"))
    ,@(when (bound-and-true-p my/jupyter-token)
        `(("jupyter-mcp-server"
           :command "uv"
           :args ("run" "jupyter-mcp-server")
           :env (:JUPYTER_URL "http://localhost:8888"
                 :JUPYTER_TOKEN ,my/jupyter-token
                 :ALLOW_IMG_OUTPUT "true"))))))

;; mcp.el is vendored as a git submodule, so there is nothing to install.
(use-package mcp
  :ensure nil
  :load-path "mcp.el/"
  :defer t
  :commands (mcp-hub mcp-hub-start-all-server mcp-hub-close-all-server)
  :config (require 'mcp-hub))

(with-eval-after-load 'mcp-hub
  (setq mcp-hub-servers (my/mcp-servers)))

(provide 'init-mcp)
;;; init-mcp.el ends here
