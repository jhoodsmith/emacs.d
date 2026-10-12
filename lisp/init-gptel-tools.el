;;; init-gptel-tools.el --- Tools the LLM can call through gptel  -*- lexical-binding: t -*-

;;; Commentary:

;; Every tool works relative to the current Projectile project root.  The
;; model chooses the arguments, so nothing here goes through a shell:
;;
;; - external programs receive an argument list (never an interpolated string);
;; - file paths are checked to stay inside the project;
;; - git refs and pathspecs are validated so they can't be read as options;
;; - destructive or outward-facing tools ask for confirmation.
;;
;; Loaded from the :config block of the gptel `use-package' in init-gptel.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel)
(require 'eww)                          ; for `eww-highest-readability'
(require 'shr)

;; Defined in db-connections.el, if present.  Declared without a value so
;; that file's own definition is not shadowed.
(defvar my/gptel-databases)
(declare-function projectile-project-root "projectile")

(defcustom my/gptel-run-command-confirm t
  "Whether the `my_run_command' tool asks before running a command.
Even whitelisted programs such as python, node and npm can run arbitrary
code, so the default is to ask."
  :type 'boolean
  :group 'gptel)

(defconst my/gptel--max-output 100000
  "Longest command output, in characters, handed back to the model.")

;;;; Helpers

(defun my/gptel--truncate (string &optional limit)
  "Truncate STRING to LIMIT characters (default `my/gptel--max-output')."
  (let ((limit (or limit my/gptel--max-output)))
    (if (> (length string) limit)
        (concat (substring string 0 limit) "\n...[truncated]")
      string)))

(defun my/gptel--root ()
  "Return the current project root as a directory name, or signal an error."
  (let ((root (and (fboundp 'projectile-project-root)
                   (projectile-project-root))))
    (unless root
      (error "Not inside a project"))
    (file-name-as-directory (expand-file-name root))))

(defun my/gptel--resolve (rel)
  "Expand REL against the project root, signalling an error if it escapes it.
Symlinks are resolved, so a link pointing outside the project is rejected."
  (let* ((root (my/gptel--root))
         (full (expand-file-name rel root)))
    (unless (file-in-directory-p full root)
      (error "Path escapes the project root: %s" rel))
    full))

(defun my/gptel--rel (full)
  "Return FULL, an absolute path inside the project, relative to the root."
  (file-relative-name full (my/gptel--root)))

(cl-defun my/gptel--run (program args &key dir env (limit my/gptel--max-output))
  "Run PROGRAM with the list of strings ARGS, without a shell.
Run in DIR (default the project root) with the strings in ENV added to the
environment.  Return (EXIT-CODE . OUTPUT), with OUTPUT cut to LIMIT."
  (unless (executable-find program)
    (error "Program not found: %s" program))
  (let ((default-directory (or dir (my/gptel--root)))
        (process-environment (append env process-environment)))
    (with-temp-buffer
      (let ((exit (apply #'process-file program nil t nil args)))
        (cons exit (my/gptel--truncate (buffer-string) limit))))))

(defun my/gptel--git (&rest args)
  "Run git with ARGS in the project root and return its output."
  (pcase-let ((`(,exit . ,out)
               (my/gptel--run "git" args
                              :env '("GIT_PAGER=cat" "GIT_TERMINAL_PROMPT=0"))))
    (cond ((not (eql exit 0))
           (format "%s\n[git exited with status %s]" out exit))
          ((string-empty-p out) "(no output)")
          (t out))))

(defconst my/gptel--ref-regexp
  "\\`[[:alnum:]_][[:alnum:]_./@{}~^-]*\\'"
  "Allowed shape of a git ref, range or remote name.
It must start with a letter, digit or underscore so it can't be read as an
option, and excludes `:' and `+' so it can't be turned into a refspec.")

(defun my/gptel--check-ref (ref what)
  "Return REF if it is a plausible git ref, else signal an error about WHAT."
  (unless (and (stringp ref) (string-match-p my/gptel--ref-regexp ref))
    (error "Invalid %s: %S" what ref))
  ref)

(defun my/gptel--check-pathspec (spec)
  "Return SPEC if it is a safe git pathspec relative to the project."
  (when (or (not (stringp spec))
            (string-empty-p spec)
            (string-prefix-p "-" spec)
            (string-prefix-p "/" spec)
            (string-prefix-p "~" spec)
            (string-prefix-p ":" spec)  ; magic pathspecs
            (string-match-p "\\(?:\\`\\|/\\)\\.\\.\\(?:/\\|\\'\\)" spec))
    (error "Invalid pathspec: %S" spec))
  spec)

(defconst my/gptel--command-env-map
  '(("pytest" . ("uv" "run"))
    ("ruff" . ("uv" "run"))
    ("mypy" . ("uv" "run"))
    ("python" . ("uv" "run"))
    ("rspec" . ("bundle" "exec"))
    ("ruby" . ("bundle" "exec"))
    ("rubocop" . ("bundle" "exec"))
    ("go")
    ("npm")
    ("node")
    ("helm"))
  "Programs `my_run_command' may run, with the launcher each needs.")

;;;; Database connections

(let ((db-config (expand-file-name "db-connections.el" user-emacs-directory)))
  (when (file-exists-p db-config)
    (load db-config nil t)))

(defun my/gptel-tool-query-database (db-name query)
  "Run QUERY against the database connection named DB-NAME.
The password goes in the environment, never on the command line."
  (let* ((db (or (cl-find db-name my/gptel-databases
                          :key (lambda (d) (plist-get d :name))
                          :test #'string=)
                 (error "Unknown database: %s" db-name)))
         (engine (plist-get db :engine))
         (host (plist-get db :host))
         (port (format "%s" (plist-get db :port)))
         (database (plist-get db :database))
         (user (plist-get db :user))
         (password (or (auth-source-pick-first-password
                        :host (plist-get db :auth-host))
                       ""))
         ;; The password goes in the environment, not on the command line
         ;; where `ps' would show it.
         (result
          (pcase engine
            ('mysql
             (my/gptel--run "mysql"
                            (list "-h" host "-P" port "-u" user
                                  "--batch" "--silent" "-e" query database)
                            :env (list (concat "MYSQL_PWD=" password))))
            ('postgresql
             (my/gptel--run "psql"
                            (list "-h" host "-p" port "-U" user "-d" database
                                  "-c" query "--no-psqlrc" "-t" "-A")
                            :env (list (concat "PGPASSWORD=" password))))
            (_ (error "Unknown engine: %s" engine)))))
    (format "Database: %s\nQuery: %s\nExit code: %s\nOutput:\n%s"
            db-name query (car result) (cdr result))))

(gptel-make-tool
 :name "my_query_database"
 :confirm t
 :function #'my/gptel-tool-query-database
 :description "Run a SQL query against a configured database. Only read-only queries should be used."
 :args (list '(:name "db-name"
                     :type string
                     :description "Name of the database connection (e.g. 'prod-mysql', 'staging-pg')")
             '(:name "query"
                     :type string
                     :description "The SQL query to execute"))
 :category "database")

;;;; Running commands

(defun my/gptel-tool-run-command (command)
  "Run the whitelisted development COMMAND from the project root.
No shell is involved; `my/gptel--command-env-map' lists what is allowed."
  (let* ((words (split-string-and-unquote command))
         (cmd-name (car words))
         (entry (and cmd-name (assoc cmd-name my/gptel--command-env-map))))
    (if (not entry)
        (format "Error: Command '%s' is not in the whitelist. Allowed commands: %s"
                (or cmd-name "")
                (mapconcat #'car my/gptel--command-env-map ", "))
      ;; No shell: `;', `|', `&&' and friends are just literal arguments.
      (let* ((full (append (cdr entry) words))
             (result (my/gptel--run (car full) (cdr full) :limit 10000)))
        (format "Command: %s\nExit code: %s\nOutput:\n%s"
                (mapconcat #'identity full " ")
                (car result) (cdr result))))))

(gptel-make-tool
 :name "my_run_command"
 :confirm my/gptel-run-command-confirm
 :function #'my/gptel-tool-run-command
 :description "Run whitelisted development commands from the project root. Python commands run with uv run, Ruby commands with bundle exec."
 :args (list '(:name "command"
                     :type string
                     :description "The command to run. Available commands: python, pytest, ruff, mypy (Python); rspec, rubocop, ruby (Ruby); go (golang); npm (nodeJS), node (NodeJS); helm (Kubernetes). Arguments can be added after the command."))
 :category "development")

;;;; General

(gptel-make-tool
 :name "current_time"
 :function (lambda () (format-time-string "%Y-%m-%d %H:%M:%S"))
 :description "Returns current time in the format %Y-%m-%d %H:%M:%S. Always call this tool first when the current date or time is relevant to the query, including before any web searches about current events, recent information, or time-sensitive topics."
 :category "general")

;; See the "Secrets" section of init-core.el for how auth-source is set up.
(defun my/get-historical-weather (latitude longitude timestamp)
  "Get historical weather from openweathermap.org with metric units.
Arguments:
  LATITUDE: latitude coordinate of the location
  LONGITUDE: longitude coordinate of the location
  TIMESTAMP: Unix timestamp for the historical data."
  (let* ((url-request-method "GET")
         (api-key (auth-source-pick-first-password :host "api.openweathermap.org"))
         (url (format "https://api.openweathermap.org/data/3.0/onecall/timemachine?units=metric&lat=%s&lon=%s&dt=%s&appid=%s"
                      latitude longitude timestamp api-key))
         (buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (re-search-forward "^$")
          (delete-region (point-min) (point))
          (buffer-string))
      (kill-buffer buffer))))

(gptel-make-tool
 :name "get_historical_weather"
 :function (lambda (latitude longitude timestamp)
             (my/get-historical-weather latitude longitude timestamp))
 :description "Get historical weather data from OpenWeatherMap API. All units of measurement in the response are metric."
 :args (list '(:name "latitude"
                     :type string
                     :description "Latitude coordinate of the location")
             '(:name "longitude"
                     :type string
                     :description "Longitude coordinate of the location")
             '(:name "timestamp"
                     :type string
                     :description "Unix timestamp for the historical data"))
 :category "weather")

;;;; Reading files

(defun my/gptel-tool-tree-view (&optional rel-path max-depth ignore-patterns)
  "Return a `tree' listing of REL-PATH (default the project root).
MAX-DEPTH limits the depth; IGNORE-PATTERNS is a comma-separated list of
extra patterns to skip."
  (let* ((target-dir (if rel-path (my/gptel--resolve rel-path) (my/gptel--root)))
         (args (append (list "-F" "--charset=ascii")
                       (when max-depth
                         (list "-L" (format "%d" max-depth)))
                       (cl-loop for pattern in (split-string (or ignore-patterns "") "," t)
                                append (list "-I" (string-trim pattern)))
                       ;; Common ignores
                       '("-I" "node_modules" "-I" ".git" "-I" "__pycache__"
                         "-I" "*.pyc" "-I" "tmp")))
         (result (my/gptel--run "tree" args :dir target-dir)))
    (if (eql (car result) 0)
        (cdr result)
      (format "Error: tree command failed (exit code %s). Make sure 'tree' is installed.\n%s"
              (car result) (cdr result)))))

(gptel-make-tool
 :name "my_tree_view"
 :function #'my/gptel-tool-tree-view
 :description "View project directory structure using the tree command. Automatically ignores common files like node_modules, .git, __pycache__, etc."
 :args (list '(:name "rel-path"
                     :type string
                     :description "Directory path relative to project root to view (default: project root)"
                     :optional t)
             '(:name "max-depth"
                     :type integer
                     :description "Maximum depth of directory tree to display (e.g., 2 for two levels)"
                     :optional t)
             '(:name "ignore-patterns"
                     :type string
                     :description "Comma-separated list of patterns to ignore (e.g., '*.log,temp*')"
                     :optional t))
 :category "file-read")

(gptel-make-tool
 :name "my_search_project"
 :function
 (lambda (pattern &optional file-pattern case-sensitive)
   (let* ((args (append '("--no-heading" "--line-number" "--color=never")
                        (unless case-sensitive '("-i"))
                        (when file-pattern (list "-g" file-pattern))
                        (list "-e" pattern)))
          (result (my/gptel--run "rg" args)))
     (pcase (car result)
       (0 (cdr result))
       (1 "No matches found.")
       (_ (format "Error: rg exited with status %s\n%s" (car result) (cdr result))))))
 :description "Search for text pattern across project files using ripgrep (rg). Returns file paths, line numbers, and matching lines."
 :args (list '(:name "pattern"
                     :type string
                     :description "Text pattern to search for (supports regex)")
             '(:name "file-pattern"
                     :type string
                     :description "Optional glob pattern to filter files (e.g., '*.py', '*.rb')"
                     :optional t)
             '(:name "case-sensitive"
                     :type boolean
                     :description "Whether search should be case-sensitive (default: false)"
                     :optional t))
 :category "file-read")

(gptel-make-tool
 :name "my_list_directory"
 :function
 (lambda (rel-path &optional match full)
   (let ((dir (my/gptel--resolve rel-path)))
     (if (file-directory-p dir)
         (mapconcat #'identity (directory-files dir full match) "\n")
       (format "Error: %s is not a directory" rel-path))))
 :description "List the contents of a directory (relative to project root)"
 :args (list '(:name "rel-path"
                     :type string
                     :description "The directory path relative to project root (e.g., '.' or 'src/util')")
             '(:name "match"
                     :type string
                     :description "Optional regex pattern to filter files"
                     :optional t)
             '(:name "full"
                     :type boolean
                     :description "Whether to return full pathnames"
                     :optional t))
 :category "file-read")

(gptel-make-tool
 :name "my_read_file"
 :function
 (lambda (rel-filepath &optional start-line end-line)
   (let ((path (my/gptel--resolve rel-filepath)))
     (if (and (file-regular-p path) (file-readable-p path))
         (with-temp-buffer
           (insert-file-contents path)
           (let ((beg (if start-line
                          (progn (goto-char (point-min))
                                 (forward-line (1- start-line))
                                 (point))
                        (point-min)))
                 (end (if end-line
                          (progn (goto-char (point-min))
                                 (forward-line end-line)
                                 (point))
                        (point-max))))
             (buffer-substring-no-properties beg end)))
       (format "Error: Cannot read file %s" rel-filepath))))
 :args (list '(:name "rel-filepath" :type string
                     :description "Path to the file relative to project root")
             '(:name "start-line" :type integer
                     :description "First line to read (1-indexed, inclusive)"
                     :optional t)
             '(:name "end-line" :type integer
                     :description "Last line to read (1-indexed, inclusive)"
                     :optional t))
 :description "Read the contents of a text file (relative to project root)"
 :category "file-read")

;;;; Writing files

(gptel-make-tool
 :name "my_create_directory"
 :function
 (lambda (rel-path)
   (let ((full (my/gptel--resolve rel-path)))
     (if (file-exists-p full)
         (format "Directory %s already exists" rel-path)
       (make-directory full t)
       (format "Created directory %s in project" rel-path))))
 :description "Create a new directory (relative to project root)"
 :args (list '(:name "rel-path"
                     :type string
                     :description "The directory path to create relative to project root (e.g., 'src/components' or 'tests/unit')"))
 :category "file-write")

(gptel-make-tool
 :name "my_create_file"
 :function
 (lambda (rel-path filename content)
   (let ((full (my/gptel--resolve
                (concat (file-name-as-directory rel-path) filename))))
     (if (file-exists-p full)
         (format "Error: %s already exists in %s; use my_update_file or my_patch_file"
                 filename rel-path)
       (make-directory (file-name-directory full) t)
       (with-temp-file full
         (insert content))
       (format "Created file %s in project path %s" filename rel-path))))
 :description "Create a new file with the specified content (relative to project root)"
 :args (list '(:name "rel-path"
                     :type string
                     :description "The directory path relative to project root (e.g., '.' or 'src/util')")
             '(:name "filename"
                     :type string
                     :description "The name of the file to create")
             '(:name "content"
                     :type string
                     :description "The content to write to the file"))
 :category "file-write")

(gptel-make-tool
 :name "my_update_file"
 :function
 (lambda (rel-filepath content &optional append)
   (let ((path (my/gptel--resolve rel-filepath)))
     (if (not (file-regular-p path))
         (format "Error: File %s does not exist" rel-filepath)
       (with-temp-file path
         (when append
           (insert-file-contents path)
           (goto-char (point-max)))
         (insert content))
       (format "Updated file %s successfully" rel-filepath))))
 :description "Update the contents of an existing file (relative to project root)"
 :args (list '(:name "rel-filepath"
                     :type string
                     :description "Path to the file relative to project root (e.g., 'README.md' or 'src/main.py')")
             '(:name "content"
                     :type string
                     :description "The content to write to the file")
             '(:name "append"
                     :type boolean
                     :description "Whether to append to the file instead of replacing content"
                     :optional t))
 :category "file-write")

(defun my/gptel-tool-patch-file (rel-filepath old-content new-content)
  "Replace the one exact occurrence of OLD-CONTENT in REL-FILEPATH.
The text is replaced with NEW-CONTENT.  Refuse if it is missing or ambiguous."
  (let ((path (my/gptel--resolve rel-filepath))
        ;; `search-forward' honours this; the block must match exactly.
        (case-fold-search nil))
    (cond
     ((not (file-regular-p path))
      (format "Error: File %s does not exist" rel-filepath))
     ((string-empty-p old-content)
      "Error: old-content must not be empty")
     (t
      (with-temp-buffer
        (insert-file-contents path)
        (let ((matches 0))
          (goto-char (point-min))
          (while (search-forward old-content nil t)
            (setq matches (1+ matches)))
          (cond
           ((= matches 0)
            (format "Error: Could not find the specified content in %s" rel-filepath))
           ((> matches 1)
            (format "Error: Found %d matches in %s — too ambiguous to patch safely"
                    matches rel-filepath))
           (t
            (goto-char (point-min))
            (search-forward old-content)
            (replace-match new-content t t)
            (write-region (point-min) (point-max) path nil 'silent)
            (format "Patched %s successfully" rel-filepath)))))))))

(gptel-make-tool
 :name "my_patch_file"
 :function #'my/gptel-tool-patch-file
 :description "Make a targeted edit to an existing file by replacing a specific block of text. Read the file first to ensure you have the exact current content."
 :args (list '(:name "rel-filepath"
                     :type string
                     :description "Path to the file relative to project root (e.g., 'src/main.py')")
             '(:name "old-content"
                     :type string
                     :description "The exact block of text to find and replace. Must match precisely.")
             '(:name "new-content"
                     :type string
                     :description "The new content to replace the old block with."))
 :category "file-write")

(defun my/gptel-tool-find-and-replace (pattern replacement &optional file-pattern)
  "Replace regexp PATTERN with REPLACEMENT in project files.
Files are found with ripgrep, optionally restricted by the glob FILE-PATTERN,
and edited in place with sd.  No shell is involved."
  (let* ((found (my/gptel--run "rg"
                               (append '("-l")
                                       (when file-pattern (list "--glob" file-pattern))
                                       (list "-e" pattern))
                               :limit most-positive-fixnum))
         (files (and (eql (car found) 0)
                     (split-string (cdr found) "\n" t))))
    (cond
     ((> (car found) 1)
      (format "Error: rg exited with status %s\n%s" (car found) (cdr found)))
     ((null files)
      (format "No files found matching pattern: %s" pattern))
     (t
      (mapconcat
       (lambda (file)
         (let ((result (my/gptel--run "sd"
                                      (list "--" pattern replacement
                                            (my/gptel--resolve file)))))
           (format "%s: %s" file
                   (if (eql (car result) 0)
                       "OK"
                     (format "error (exit %s) %s" (car result) (cdr result))))))
       files "\n")))))

(gptel-make-tool
 :name "my_find_replace"
 :confirm t
 :function #'my/gptel-tool-find-and-replace
 :description "Find and replace text across project files using ripgrep (to find) and sd (to replace). sd uses regex by default."
 :args (list '(:name "pattern"
                     :type string
                     :description "The regex pattern to search for")
             '(:name "replacement"
                     :type string
                     :description "The replacement string. Use $1, $2 etc. for capture groups.")
             '(:name "file-pattern"
                     :type string
                     :description "Optional glob pattern to restrict which files are modified (e.g., '*.py', '*.ts')"
                     :optional t))
 :category "file-write")

(gptel-make-tool
 :name "my_rename_file"
 :function
 (lambda (old-rel-path new-rel-path)
   (let ((old (my/gptel--resolve old-rel-path))
         (new (my/gptel--resolve new-rel-path)))
     (cond
      ((not (file-exists-p old))
       (format "Error: Source file %s does not exist" old-rel-path))
      ((file-exists-p new)
       (format "Error: Destination %s already exists" new-rel-path))
      (t
       (make-directory (file-name-directory new) t)
       (rename-file old new)
       (format "Renamed %s to %s" old-rel-path new-rel-path)))))
 :description "Rename or move a file within the current project"
 :args (list '(:name "old-rel-path"
                     :type string
                     :description "Current path of the file relative to project root")
             '(:name "new-rel-path"
                     :type string
                     :description "New path for the file relative to project root"))
 :category "file-write")

(gptel-make-tool
 :name "my_copy_file"
 :function
 (lambda (source-rel-path dest-rel-path)
   (let ((source (my/gptel--resolve source-rel-path))
         (dest (my/gptel--resolve dest-rel-path)))
     (cond
      ((not (file-exists-p source))
       (format "Error: Source file %s does not exist" source-rel-path))
      ((file-directory-p source)
       (format "Error: %s is a directory, not a file" source-rel-path))
      ((file-exists-p dest)
       (format "Error: Destination %s already exists" dest-rel-path))
      (t
       (make-directory (file-name-directory dest) t)
       (copy-file source dest)
       (format "Copied %s to %s" source-rel-path dest-rel-path)))))
 :description "Copy a file within the current project"
 :args (list '(:name "source-rel-path"
                     :type string
                     :description "Path of the source file relative to project root")
             '(:name "dest-rel-path"
                     :type string
                     :description "Path for the destination file relative to project root"))
 :category "file-write")

(gptel-make-tool
 :name "my_delete_file"
 :confirm t
 :function
 (lambda (rel-filepath)
   (let ((full (my/gptel--resolve rel-filepath)))
     (cond
      ((not (file-exists-p full))
       (format "Error: File %s does not exist" rel-filepath))
      ((file-directory-p full)
       (format "Error: %s is a directory, use my_delete_directory instead" rel-filepath))
      (t
       (delete-file full)
       (format "Deleted file %s" rel-filepath)))))
 :description "Delete a file from the current project"
 :args (list '(:name "rel-filepath"
                     :type string
                     :description "Path to the file relative to project root (e.g., 'src/old_file.py')"))
 :category "file-write")

(defun my/gptel-tool-delete-directory (rel-path &optional recursive)
  "Delete the project directory REL-PATH, with its contents if RECURSIVE.
The project root itself is never deleted."
  (let ((full (my/gptel--resolve rel-path)))
    (cond
     ((file-equal-p full (my/gptel--root))
      "Error: Refusing to delete the project root")
     ((not (file-exists-p full))
      (format "Error: Directory %s does not exist" rel-path))
     ((not (file-directory-p full))
      (format "Error: %s is not a directory" rel-path))
     (recursive
      (delete-directory full t)
      (format "Deleted directory %s and all contents" rel-path))
     ((directory-files full nil "^[^.]")
      (format "Error: Directory %s is not empty. Use recursive=true to delete with contents" rel-path))
     (t
      (delete-directory full)
      (format "Deleted empty directory %s" rel-path)))))

(gptel-make-tool
 :name "my_delete_directory"
 :confirm t
 :function #'my/gptel-tool-delete-directory
 :description "Delete a directory from the current project"
 :args (list '(:name "rel-path"
                     :type string
                     :description "Path to the directory relative to project root (e.g., 'old_folder')")
             '(:name "recursive"
                     :type boolean
                     :description "Whether to delete directory and all its contents"
                     :optional t))
 :category "file-write")

;;;; Web

(defun my/gptel-tool-fetch-webpage (url &optional raw)
  "Fetch URL and return its readable text, or the raw HTML if RAW.
Only http and https URLs are allowed."
  (if (not (string-match-p "\\`https?://" url))
      ;; Without this, file: URLs would read arbitrary local files.
      "Error: Only http and https URLs can be fetched"
    (with-temp-buffer
      (condition-case err
          (progn
            (url-insert-file-contents url)
            (if raw
                (buffer-string)
              (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
                     (best-node (eww-highest-readability dom)))
                (erase-buffer)
                (shr-insert-document best-node)
                (buffer-substring-no-properties (point-min) (point-max)))))
        (error (format "Error fetching %s: %s" url (error-message-string err)))))))

(gptel-make-tool
 :name "my_fetch_webpage"
 :function #'my/gptel-tool-fetch-webpage
 :description "Fetch and extract readable text content from a webpage URL"
 :args (list '(:name "url"
                     :type string
                     :description "The URL to fetch (e.g., 'https://example.com')")
             '(:name "raw"
                     :type boolean
                     :description "Return raw HTML instead of extracted readable text"
                     :optional t))
 :category "web")

(defun my/gptel-tool-web-search (query &optional count)
  "Search the web for QUERY via Brave Search; return COUNT results (default 5)."
  (let* ((api-key (auth-source-pick-first-password :host "api.search.brave.com"))
         (url (format "https://api.search.brave.com/res/v1/web/search?q=%s&count=%d"
                      (url-hexify-string query)
                      (or count 5)))
         (url-request-extra-headers
          `(("Accept" . "application/json")
            ("X-Subscription-Token" . ,api-key))))
    (with-temp-buffer
      (url-insert-file-contents url)
      (let* ((json (json-parse-buffer :object-type 'plist))
             (results (plist-get (plist-get json :web) :results)))
        (mapconcat
         (lambda (r)
           (format "Title: %s\nURL: %s\nDescription: %s\n"
                   (plist-get r :title)
                   (plist-get r :url)
                   (plist-get r :description)))
         results "\n---\n")))))

(gptel-make-tool
 :name "my_web_search"
 :function #'my/gptel-tool-web-search
 :description "Search the web using Brave Search API."
 :args (list '(:name "query"
                     :type string
                     :description "Search query")
             '(:name "count"
                     :type integer
                     :description "Number of results (default 5)"
                     :optional t))
 :category "web")

;;;; Git

(gptel-make-tool
 :name "git_status"
 :function (lambda () (my/gptel--git "status" "--porcelain"))
 :description "Show git status for current project"
 :category "git")

(gptel-make-tool
 :name "git_branch"
 :function (lambda () (my/gptel--git "branch" "-v"))
 :description "Show current branch and recent commit info"
 :category "git")

(gptel-make-tool
 :name "git_remote"
 :function (lambda () (my/gptel--git "remote" "-v"))
 :description "Show git remote repositories"
 :category "git")

(gptel-make-tool
 :name "git_diff"
 :function
 (lambda (&optional file-path staged ref)
   (apply #'my/gptel--git "diff" "--no-ext-diff"
          (append (when staged '("--cached"))
                  (when ref (list (my/gptel--check-ref ref "ref")))
                  (when file-path
                    (list "--" (my/gptel--rel (my/gptel--resolve file-path)))))))
 :description "Show git diff. Can show unstaged changes, staged changes, or diff against a ref/branch."
 :args (list '(:name "file-path"
                     :type string
                     :description "Optional specific file to diff (relative to project root)"
                     :optional t)
             '(:name "staged"
                     :type boolean
                     :description "Show staged changes instead of unstaged"
                     :optional t)
             '(:name "ref"
                     :type string
                     :description "Git ref to diff against (e.g., 'main', 'HEAD~3', 'branch1..branch2')"
                     :optional t))
 :category "git")

(gptel-make-tool
 :name "git_log"
 :function
 (lambda (&optional max-count file-path)
   (apply #'my/gptel--git "log" "--oneline" "-n" (format "%d" (or max-count 10))
          (when file-path
            (list "--" (my/gptel--rel (my/gptel--resolve file-path))))))
 :description "Show git commit history (default 10 commits)"
 :args (list '(:name "max-count"
                     :type integer
                     :description "Maximum number of commits to show"
                     :optional t)
             '(:name "file-path"
                     :type string
                     :description "Show history for specific file only"
                     :optional t))
 :category "git")

(gptel-make-tool
 :name "git_show"
 :function
 (lambda (commit-hash &optional file-path)
   (apply #'my/gptel--git "show" "--no-ext-diff"
          (my/gptel--check-ref commit-hash "commit")
          (when file-path
            (list "--" (my/gptel--rel (my/gptel--resolve file-path))))))
 :description "Show details of a specific commit"
 :args (list '(:name "commit-hash"
                     :type string
                     :description "Git commit hash or reference (e.g., 'HEAD', 'main')")
             '(:name "file-path"
                     :type string
                     :description "Show changes for specific file only"
                     :optional t))
 :category "git")

(gptel-make-tool
 :name "git_blame"
 :function
 (lambda (file-path &optional start-line end-line)
   (apply #'my/gptel--git "blame"
          (append (when (and start-line end-line)
                    (list "-L" (format "%d,%d" start-line end-line)))
                  (list "--" (my/gptel--rel (my/gptel--resolve file-path))))))
 :description "Show who last modified each line of a file, with commit hash and timestamp."
 :args (list '(:name "file-path"
                     :type string
                     :description "Path to the file relative to project root")
             '(:name "start-line"
                     :type integer
                     :description "Start of line range to blame"
                     :optional t)
             '(:name "end-line"
                     :type integer
                     :description "End of line range to blame"
                     :optional t))
 :category "git")

(gptel-make-tool
 :name "git_file_at_commit"
 :function
 (lambda (commit-hash file-path)
   (my/gptel--git "show"
                  (format "%s:./%s"
                          (my/gptel--check-ref commit-hash "commit")
                          (my/gptel--rel (my/gptel--resolve file-path)))))
 :description "Show the full contents of a file at a specific commit."
 :args (list '(:name "commit-hash"
                     :type string
                     :description "Git commit hash or reference (e.g., 'HEAD', 'main', 'abc1234')")
             '(:name "file-path"
                     :type string
                     :description "Path to the file relative to project root"))
 :category "git")

(gptel-make-tool
 :name "git_add"
 :function
 (lambda (pathspec)
   (my/gptel--git "add" "--" (my/gptel--check-pathspec pathspec)))
 :description "Stage files for commit"
 :args (list '(:name "pathspec"
                     :type string
                     :description "File path or pattern to stage (e.g., 'src/foo.py', '.'  for all)"))
 :category "git")

(gptel-make-tool
 :name "git_commit"
 :confirm t
 :function (lambda (message) (my/gptel--git "commit" "-m" message))
 :description "Create a git commit from staged changes"
 :args (list '(:name "message"
                     :type string
                     :description "Commit message"))
 :category "git")

(gptel-make-tool
 :name "git_push"
 :confirm t
 :function
 (lambda (&optional remote branch)
   (apply #'my/gptel--git "push"
          (my/gptel--check-ref (or remote "origin") "remote")
          (when (and branch (not (string-empty-p branch)))
            (list (my/gptel--check-ref branch "branch")))))
 :description "Push commits to a remote repository"
 :args (list '(:name "remote"
                     :type string
                     :description "Remote name (default: origin)"
                     :optional t)
             '(:name "branch"
                     :type string
                     :description "Branch to push (default: current branch)"
                     :optional t))
 :category "git")

(gptel-make-tool
 :name "git_checkout"
 :function
 (lambda (branch &optional create)
   (apply #'my/gptel--git "checkout"
          (append (when create '("-b"))
                  (list (my/gptel--check-ref branch "branch")))))
 :description "Switch to a branch, or create and switch to a new one"
 :args (list '(:name "branch"
                     :type string
                     :description "Branch name to switch to or create")
             '(:name "create"
                     :type boolean
                     :description "Create the branch if it doesn't exist"
                     :optional t))
 :category "git")

;;;; Emacs

(defun my/gptel-tool-eval-elisp (expression &optional buffer-name)
  "Evaluate EXPRESSION once, optionally in BUFFER-NAME, and describe the result."
  (condition-case err
      (let* ((target (if buffer-name
                         (or (get-buffer buffer-name)
                             (error "No such buffer: %s" buffer-name))
                       (current-buffer)))
             (result nil)
             ;; Evaluate once, capturing printed output and the value.
             (output (with-output-to-string
                       (setq result
                             (with-current-buffer target
                               (eval (read expression) t))))))
        (concat (format "Result: %S\nType: %s" result (type-of result))
                (unless (string-empty-p output)
                  (format "\nOutput: %s" output))))
    (error (format "Error: %s" (error-message-string err)))))

(gptel-make-tool
 :name "my_eval_elisp"
 :confirm t
 :function #'my/gptel-tool-eval-elisp
 :description "Evaluate an Emacs Lisp expression and return the result, its type, and any printed output. Optionally evaluate in the context of a specific buffer."
 :args (list '(:name "expression"
                     :type string
                     :description "The Emacs Lisp expression to evaluate")
             '(:name "buffer-name"
                     :type string
                     :description "Optional buffer name to evaluate the expression in"
                     :optional t))
 :category "emacs")

(provide 'init-gptel-tools)
;;; init-gptel-tools.el ends here
