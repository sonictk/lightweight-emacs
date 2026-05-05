;; -*- lexical-binding: t; -*-
(require 'gnutls)
(require 'json)
(require 'p4)
(require 'project)
(require 'url)

                                        ; TODO This is all Windows-development-specific. Might need to eventually make this work on other platforms if needed.

(defun epic-p4-safe-backout-changelist ()
  "Backout/undo a previously-submitted changelist. (Epic-specific.)"
  (interactive)
  (let* ((localappdata (getenv "LOCALAPPDATA"))
         (p4client (getenv "P4CLIENT"))
         (cl (p4-completing-read 'submitted "Changelist: "))
         (buffer-name "*Epic Safe Backout Tool*"))
    (let ((executable (concat localappdata "\\Epic Games\\P4VUtils\\P4VUtils.exe")))
      (with-current-buffer (get-buffer-create buffer-name)
        (setq buffer-read-only t)
        (display-buffer (current-buffer))
        (let ((proc (start-process "Epic-SafeBackoutTool" buffer-name executable
                                   "backout" cl p4client)))
          (set-process-sentinel proc
                                (lambda (process event)
                                  (when (string= event "finished\n")
                                    (setq buffer-read-only nil)
                                    (with-current-buffer buffer-name
                                      (insert "\n[Process completed]\n"))
                                    (setq buffer-read-only t)))))))))

(defun epic-p4-safe-restore-changelist ()
  "Restore a previously-backed-out changelist. (Epic-specific.)"
  (interactive)
  (let* ((localappdata (getenv "LOCALAPPDATA"))
         (p4client (getenv "P4CLIENT"))
         (cl (p4-completing-read 'submitted "Changelist: "))
         (buffer-name "*Epic Safe Restore Tool*"))
    (let ((executable (concat localappdata "\\Epic Games\\P4VUtils\\P4VUtils.exe")))
      (with-current-buffer (get-buffer-create buffer-name)
        (setq buffer-read-only t)
        (display-buffer (current-buffer))
        (let ((proc (start-process "Epic-SafeRestoreTool" buffer-name executable
                                   "restore" cl p4client)))
          (set-process-sentinel proc
                                (lambda (process event)
                                  (when (string= event "finished\n")
                                    (setq buffer-read-only nil)
                                    (with-current-buffer buffer-name
                                      (insert "\n[Process completed]\n"))
                                    (setq buffer-read-only t)))))))))

(defun epic-launch-submit-tool ()
  "Launch Epic's SubmitTool."
  (interactive)
  (let* ((p4port (getenv "P4PORT"))
         (p4user (getenv "P4USER"))
         (p4client (getenv "P4CLIENT"))
         (localappdata (getenv "LOCALAPPDATA"))
         (root-dir (string-trim-right (shell-command-to-string "p4 -F %clientRoot% -ztag info")))
         (cl (p4-completing-read 'pending "Changelist: "))
         (buffer-name "*Epic SubmitTool*"))
    (let ((executable (concat localappdata "\\UnrealGameSync\\Tools\\SubmitTool\\Current\\Windows\\Engine\\Binaries\\Win64\\SubmitTool.exe")))
      (with-current-buffer (get-buffer-create buffer-name)
        (setq buffer-read-only t)
        (display-buffer (current-buffer))
        (let ((proc (start-process "Epic-SubmitTool" buffer-name executable
                                   "-server" p4port
                                   "-user" p4user
                                   "-client" p4client
                                   "-root-dir" root-dir
                                   "-cl" cl)))
          (set-process-sentinel proc
                                (lambda (process event)
                                  (when (string= event "finished\n")
                                    (setq buffer-read-only nil)
                                    (with-current-buffer buffer-name
                                      (insert "\n[Process completed]\n"))
                                    (setq buffer-read-only t))))))))
  (with-current-buffer "*Epic SubmitTool*"
    (local-set-key "q" (lambda () (interactive) (quit-window t)))))

(defun epic-preflight-changelist ()
  "Preflight a changelist on Horde."
  (interactive)
  (let* ((stream-name (string-trim-right (shell-command-to-string "p4 -F \"%Stream%\" -ztag client -o")))
         (cl (p4-completing-read 'shelved "Changelist: ")))
    (browse-url (concat "https://horde.devtools.epicgames.com/preflight?stream="
                        (url-hexify-string stream-name)
                        "&change="
                        (url-hexify-string cl)))))

                                        ; This gets `project.el` to recognize Unreal Engine workspaces as project roots.
(defcustom project-root-markers
  '("Default.uprojectdirs") ; This file is always present in any Unreal root workspace.
  "Files or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)

(defun project-root-p (path)
  "Check if the current PATH has any of the project root markers."
  (catch 'found
    (dolist (marker project-root-markers)
      (when (file-exists-p (concat path marker))
        (throw 'found marker)))))

(defun project-find-root (path)
  "Search up the PATH for `project-root-markers'."
  (when-let* ((root (locate-dominating-file path #'project-root-p)))
    (cons 'transient (expand-file-name root))))
                                        ; This really slows down project-files due to the sheer size of the project. Need to be careful here.
                                        ; This can affect things like eglot as well for languages that support workspace symbols.
(add-to-list 'project-find-functions #'project-find-root)

                                        ; Allow projectile to recognize the root of the Unreal workspace.
;; (with-eval-after-load 'projectile
;;   (projectile-register-project-type 'ue5 '("Default.uprojectdirs")
;;                                     :project-file "Default.uprojectdirs"))

(defun find-public-or-private-directory (path)
  "Find either 'Public' or 'Private' directory presence."
  (or (locate-dominating-file path "Public")
      (locate-dominating-file path "Private")
      (locate-dominating-file path "Classes")))

(defun trim-first-component (path)
  "Trim the first component of the path."
  (let* ((components (split-string path "/"))
         (remaining-components (cdr components)))
    (if remaining-components
        (mapconcat 'identity remaining-components "/")
      path)))

(defun ue-ff-other-file-alist-function ()
  "Makes Unreal Engine directory structure work with `find-file.el`."
  (when (buffer-file-name)
    (let* ((filename (buffer-file-name))
           (file-dir (file-name-directory filename))
           (module-root-dir (find-public-or-private-directory file-dir))
           (module-relative-path (file-relative-name filename module-root-dir))
           (module-relative-path (trim-first-component module-relative-path))
           (module-relative-path (file-name-directory module-relative-path))
           (public-dir (concat module-root-dir "Public/" module-relative-path))
           (private-dir (concat module-root-dir "Private/" module-relative-path))
           (classes-dir (concat module-root-dir "Classes/" module-relative-path)))
      (setq cc-search-directories (nconc cc-search-directories `(,public-dir ,private-dir ,classes-dir))))))

(defun ue-ff-restore-search-directories ()
  (custom-reevaluate-setting 'cc-search-directories))

(defun ue-get-solution-path (interactive)
  "Find the first .sln file in the project root."
  (let ((root (project-root (project-current))))
    (directory-files root t "\\.sln$" t)))

(defcustom epic-codescout-cookie nil
  "The authentication cookie for Epic's CodeScout.
Set this to the full 'connect.sid=...' string."
  :type 'string
  :group 'tools)

(defun epic-codescout--highlight-match (line start len)
  "Create a string with the matched part highlighted using the theme's 'match' face."
  (if (and (integerp start) (integerp len) (<= 0 start) (<= (+ start len) (length line)))
      (concat
       (substring line 0 start)
       (propertize (substring line start (+ start len)) 'font-lock-face 'match)
       (substring line (+ start len)))
    (message "CodeScout: Received invalid highlight data for line: %s" line)
    line))

(defvar-local epic-codescout--search-term nil)
(defvar-local epic-codescout--search-path-filter nil)
(defvar-local epic-codescout--search-branch "//Fortnite/Main")
(defvar-local epic-codescout--p4-root-cache nil)
(defvar-local epic-codescout--line-meta nil
  "Hash table mapping buffer line numbers to (file-path . line-num) cons cells.")

(defun epic-codescout--p4-client-root ()
  "Return the local root of the current Perforce workspace, cached per buffer."
  (or epic-codescout--p4-root-cache
      (condition-case err
          (let ((raw (string-trim (shell-command-to-string "p4 -F %clientRoot% -ztag info"))))
            (unless (string-empty-p raw)
              (setq-local epic-codescout--p4-root-cache
                          (expand-file-name raw))))
        (error (message "CodeScout: could not get P4 client root: %s" err) nil))))

(defun epic-codescout--open-local (rel-path line-num)
  "Open REL-PATH (relative to the P4 client root) at LINE-NUM in another window."
  (let* ((root       (epic-codescout--p4-client-root))
         (local-path (when root (concat (file-name-as-directory root) rel-path))))
    (cond
     ((and local-path (file-exists-p local-path))
      (find-file-other-window local-path)
      (goto-char (point-min))
      (forward-line (1- line-num)))
     (local-path
      (message "CodeScout: file not found locally: %s" local-path))
     (t
      (message "CodeScout: could not determine P4 root")))))

(defun epic-codescout-browse-result-at-point ()
  "Open the current CodeScout search in the browser."
  (interactive)
  (browse-url
   (concat "https://codescout.internal.epicgames.net/?"
           (url-build-query-string
            `(("branch"    ,epic-codescout--search-branch)
              ("searchStr" ,(or epic-codescout--search-term ""))
              ("pathsStr"  ,(or epic-codescout--search-path-filter ""))
              ("options"   "{\"matchCase\":false,\"wholeWord\":false}"))))))

(defun epic-codescout-open-result-at-point ()
  "Open the local file for the result at point (used for RET and mouse-1)."
  (interactive)
  (let ((meta (and epic-codescout--line-meta
                   (gethash (line-number-at-pos) epic-codescout--line-meta))))
    (if meta
        (epic-codescout--open-local (car meta) (cdr meta))
      (message "No result at point"))))

(defun epic-codescout--format-results (results-buffer data search-term path-filter branch)
  "Format the parsed JSON DATA (an alist) into the RESULTS-BUFFER using grep-mode."
  (with-current-buffer results-buffer
    (let ((inhibit-read-only t)
          (num-matches (cdr (assoc 'numMatches data)))
          (is-truncated (let ((val (cdr (assoc 'resultsTruncated data))))
                          (if (eq val :json-false) nil val)))
          (match-count 1))
      (erase-buffer)
      (special-mode)
      (setq-local epic-codescout--search-term        search-term)
      (setq-local epic-codescout--search-path-filter path-filter)
      (setq-local epic-codescout--search-branch      (or branch "//Fortnite/Main"))
      (setq-local epic-codescout--line-meta (make-hash-table :test 'eql))
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map special-mode-map)
        (define-key map (kbd "RET") #'epic-codescout-open-result-at-point)
        (define-key map (kbd "q")   #'quit-window)
        (use-local-map map))
      (insert (format "%s matches found.%s\n\n"
                      num-matches
                      (if is-truncated " (Results Truncated)" "")))
      (dolist (file-match (coerce (cdr (assoc 'fileMatches data)) 'list))
        (let* ((file-path (cdr (assoc 'file file-match)))
               (line-matches (coerce (cdr (assoc 'lineMatches file-match)) 'list)))
          (dolist (line-match line-matches)
            (let* ((line-num        (cdr (assoc 'lineNumber line-match)))
                   (line-str        (cdr (assoc 'lineStr line-match)))
                   (match-start     (cdr (assoc 'matchStart line-match)))
                   (match-len       (cdr (assoc 'matchLen line-match)))
                   (highlighted-str (epic-codescout--highlight-match line-str match-start match-len))
                   (eff-line        (if (numberp line-num) line-num
                                      (string-to-number (format "%s" line-num)))))
              (puthash (line-number-at-pos) (cons file-path eff-line)
                       epic-codescout--line-meta)
              (insert (format "%d. " match-count))
              ;; path:line → opens file locally
              (insert-text-button
               (format "%s:%s" file-path line-num)
               'face 'compilation-info
               'follow-link t
               'mouse-face 'highlight
               'help-echo "mouse-1: open in Emacs"
               'action (lambda (_btn)
                         (epic-codescout-open-result-at-point)))
              (insert ":")
              ;; match content → opens CodeScout in browser
              (insert-text-button
               highlighted-str
               'face 'link
               'follow-link t
               'mouse-face 'highlight
               'help-echo "mouse-1: open search in browser"
               'action (lambda (_btn) (epic-codescout-browse-result-at-point)))
              (insert "\n")
              (incf match-count))))))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun epic-codescout--response-is-auth-failure-p ()
  "Return non-nil if the current buffer contains an HTML login page rather than JSON."
  (save-excursion
    (goto-char (point-min))
    (looking-at-p "<!DOCTYPE\\|<html")))

(defun epic-codescout--try-read-firefox-cookie ()
  "Try to read the current connect.sid cookie for CodeScout from Firefox.
Requires sqlite3 on PATH.  Returns \"connect.sid=VALUE\" or nil."
  (when (executable-find "sqlite3")
    (let* ((profiles-dir (expand-file-name "Mozilla/Firefox/Profiles" (getenv "APPDATA")))
           (dbs (when (file-directory-p profiles-dir)
                  (seq-filter #'file-exists-p
                              (mapcar (lambda (entry)
                                        (expand-file-name
                                         "cookies.sqlite"
                                         (expand-file-name entry profiles-dir)))
                                      (directory-files profiles-dir nil "^[^.]")))))
           (db  (car (sort dbs
                           (lambda (a b)
                             (time-less-p
                              (file-attribute-modification-time (file-attributes b))
                              (file-attribute-modification-time (file-attributes a))))))))
      (when db
        (let ((tmp (make-temp-file "cs-cookies" nil ".sqlite")))
          (condition-case nil (copy-file db tmp t) (error nil))
          (let ((val (string-trim
                      (shell-command-to-string
                       (format "sqlite3 %s \"SELECT value FROM moz_cookies WHERE host='codescout.internal.epicgames.net' AND name='connect.sid' ORDER BY lastAccessed DESC LIMIT 1\""
                               (shell-quote-argument tmp))))))
            (ignore-errors (delete-file tmp))
            (unless (string-empty-p val)
              (concat "connect.sid=" val))))))))

(defun epic-codescout--manual-login (search-term path-filter branch)
  "Open CodeScout in a browser for Okta login, then retry the search."
  (browse-url "https://codescout.internal.epicgames.net/")
  (when (y-or-n-p "CodeScout: log in via Okta in your browser, then press y: ")
    (let ((auto (epic-codescout--try-read-firefox-cookie)))
      (if auto
          (progn
            (customize-save-variable 'epic-codescout-cookie auto)
            (message "CodeScout: cookie read from Firefox, retrying...")
            (epic-codescout--do-search search-term path-filter branch t))
        (let ((val (read-string
                    "Paste connect.sid value (Firefox DevTools -> Storage -> Cookies -> codescout): ")))
          (unless (string-empty-p val)
            (let ((cookie (if (string-prefix-p "connect.sid=" val) val
                            (concat "connect.sid=" val))))
              (customize-save-variable 'epic-codescout-cookie cookie)
              (message "CodeScout: cookie updated, retrying...")
              (epic-codescout--do-search search-term path-filter branch t))))))))

(defun epic-codescout--reauthenticate (search-term path-filter branch)
  "Acquire a fresh CodeScout cookie and retry the search.
Reads the cookie directly from Firefox if possible.  Falls back to opening
a browser and prompting the user when auto-detection fails or is also expired.
REAUTH-STAGE threads through as nil → 'auto → t so each stage fires once."
  (let ((auto (epic-codescout--try-read-firefox-cookie)))
    (if auto
        (progn
          (customize-save-variable 'epic-codescout-cookie auto)
          (message "CodeScout: cookie refreshed from Firefox, retrying...")
          (epic-codescout--do-search search-term path-filter branch 'auto))
      (epic-codescout--manual-login search-term path-filter branch))))

(defun epic-codescout--handle-auth-failure (search-term path-filter branch reauth-stage)
  "React to a CodeScout session expiry.
REAUTH-STAGE is nil on first failure, 'auto after Firefox auto-refresh failed,
and t after manual login also failed."
  (cond
   ((eq reauth-stage t)
    (message "CodeScout: authentication still failed after manual login."))
   ((eq reauth-stage 'auto)
    (message "CodeScout: auto-refreshed cookie also expired, opening browser for manual login...")
    (epic-codescout--manual-login search-term path-filter branch))
   (t
    (message "CodeScout: session expired.")
    (epic-codescout--reauthenticate search-term path-filter branch))))

(defun epic-codescout--status-is-auth-failure-p (status)
  "Return non-nil if STATUS signals a session expiry redirect."
  (or (plist-get status :redirect)
      (eq (cadr (plist-get status :error)) 'http-redirect-limit)))

(defun epic-codescout-search-callback (status search-term path-filter branch reauth-attempted)
  "Handle the url-retrieve response for a CodeScout search."
  (cond
   ((epic-codescout--status-is-auth-failure-p status)
    (kill-buffer (current-buffer))
    (epic-codescout--handle-auth-failure search-term path-filter branch reauth-attempted))
   ((plist-get status :error)
    (kill-buffer (current-buffer))
    (message "CodeScout search failed: %s" (plist-get status :error)))
   (t
    (goto-char (point-min))
    (when (re-search-forward "\n\r?\n\r?" nil t)
      (delete-region (point-min) (point)))
    (if (epic-codescout--response-is-auth-failure-p)
        (progn
          (kill-buffer (current-buffer))
          (epic-codescout--handle-auth-failure search-term path-filter branch reauth-attempted))
      (condition-case err
          (let* ((json-key-type 'symbol)
                 (json-data     (json-read-from-string (buffer-string)))
                 (results-buf   (get-buffer-create "*codescout-search-results*")))
            (kill-buffer (current-buffer))
            (switch-to-buffer-other-window results-buf)
            (epic-codescout--format-results results-buf json-data search-term path-filter branch))
        (error
         (kill-buffer (current-buffer))
         (message "CodeScout: could not parse response: %s" err)))))))

(defun epic-codescout--do-search (search-term path-filter branch reauth-attempted)
  "Fire the CodeScout HTTP request for SEARCH-TERM filtered by PATH-FILTER on BRANCH."
  (let* ((base-url "https://codescout.internal.epicgames.net/api/find-in-files")
         (params `(("branch"    ,(or branch "//Fortnite/Main"))
                   ("searchStr" ,search-term)
                   ("pathsStr"  ,(or path-filter ""))
                   ("options"   "{\"matchCase\":false,\"wholeWord\":false}")))
         (full-url (concat base-url "?" (url-build-query-string params)))
         (url-request-extra-headers
          (when (and epic-codescout-cookie (not (string-empty-p epic-codescout-cookie)))
            `(("Cookie" . ,epic-codescout-cookie))))
         (url-max-redirections 0))
    (url-retrieve full-url
                  (lambda (status)
                    (epic-codescout-search-callback status search-term path-filter branch reauth-attempted)))))

(defun epic-codescout-search (search-term &optional path-filter branch)
  "Search Epic's CodeScout for SEARCH-TERM.
With a prefix argument (C-u), also prompt for a path/filetype filter
\(e.g. *.cpp, Engine/Source/Runtime\) and the branch to search.
If the session cookie is expired, opens the Okta login page automatically."
  (interactive
   (let ((default (when (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end)))))
     (if current-prefix-arg
         (list (read-string "Search CodeScout for: " default)
               (read-string "Filter by path/filetype (e.g. *.cpp, Engine/Source): ")
               (completing-read "Branch: " (ignore-errors (p4--fetch-stream-list))
                                nil nil "//Fortnite/Main"))
       (list (read-string "Search CodeScout for: " default)))))
  (epic-codescout--do-search search-term (or path-filter "") (or branch "//Fortnite/Main") nil))

(add-hook 'ff-pre-find-hooks 'ue-ff-other-file-alist-function)
(add-hook 'ff-post-load-hooks 'ue-ff-restore-search-directories)
(add-hook 'ff-not-found-hooks 'ue-ff-restore-search-directories)
(add-hook 'ff-file-created-hook 'ue-ff-restore-search-directories)

(provide 'epic-games-internal)

; This should be in the `.dir-locals.el` file:

;; ((nil . ((tab-width . 4)
;;          (indent-tabs-mode . nil)))

;;  (c++-ts-mode . ((c-basic-offset . 4)
;;                  (tab-width . 4)
;;                  (indent-tabs-mode . t)
;;                  (fill-column . 120)
;;                  (my-clangd-executable-path . "S:/source/repos/epic/ysiew_devvk/Engine/Restricted/NotForLicensees/Binaries/Win64/AutoRTFM/20/bin/verse-clangd.exe")
;;                  (eval . (progn
;;                            ;; 1. Define a "flag" variable and make it permanent for this buffer
;;                            (put 'my-dir-locals-initialized 'permanent-local t)
                           
;;                            ;; 2. Only run the setup if this specific buffer hasn't been initialized
;;                            (unless (bound-and-true-p my-dir-locals-initialized)
;;                              (setq-local compile-command
;;                                          (concat (locate-dominating-file buffer-file-name ".dir-locals.el")
;;                                                  "Engine/Build/BatchFiles/RunUBT.bat"))
;;                              ;; Mark this buffer as "dirty" so we don't overwrite again
;;                              (setq-local my-dir-locals-initialized t))
                           
;;                            ;; 3. Also protect compile-command itself from being wiped by reverts
;;                            (put 'compile-command 'permanent-local t)))
;;                  (eval . (setq-local cd-compile-directory
;;                                      (locate-dominating-file buffer-file-name ".dir-locals.el")))))

;;  (c-ts-mode . ((c-basic-offset . 4)
;;                (tab-width . 4)
;;                (indent-tabs-mode . t)
;;                (fill-column . 120)
;;                (my-clangd-executable-path . "S:/source/repos/epic/ysiew_devvk/Engine/Restricted/NotForLicensees/Binaries/Win64/AutoRTFM/20/bin/verse-clangd.exe")
;;                (eval . (progn
;;                          ;; 1. Define a "flag" variable and make it permanent for this buffer
;;                          (put 'my-dir-locals-initialized 'permanent-local t)
                         
;;                          ;; 2. Only run the setup if this specific buffer hasn't been initialized
;;                          (unless (bound-and-true-p my-dir-locals-initialized)
;;                            (setq-local compile-command
;;                                        (concat (locate-dominating-file buffer-file-name ".dir-locals.el")
;;                                                "Engine/Build/BatchFiles/RunUBT.bat"))
;;                            ;; Mark this buffer as "dirty" so we don't overwrite again
;;                            (setq-local my-dir-locals-initialized t))
                         
;;                          ;; 3. Also protect compile-command itself from being wiped by reverts
;;                          (put 'compile-command 'permanent-local t)))
;;                (eval . (setq-local cd-compile-directory
;;                                    (locate-dominating-file buffer-file-name ".dir-locals.el"))))))
