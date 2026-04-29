;; -*- lexical-binding: t; -*-
;;
;; JIRA integration for Emacs.
;;
;; Required environment variables:
;;   JIRAURL         - Root URL of your JIRA instance, e.g. https://example.atlassian.net
;;   JIRA_USER_EMAIL - Your Atlassian account email address.
;;   JIRA_API_TOKEN  - Atlassian API token.
;;                     Generate one at:
;;                     https://id.atlassian.com/manage-profile/security/api-tokens

(require 'json)
(require 'url)
(require 'tabulated-list)

(defvar jira--buffer-name "*JIRA: My Filed Issues*")
(defvar jira--page-size 100
  "Number of issues to request per page (max allowed by JIRA Cloud is 100).")

;; --- Authentication ---------------------------------------------------------

(defun jira--auth-header ()
  "Return a Basic Authorization header value for JIRA Cloud requests."
  (concat "Basic "
          (base64-encode-string
           (concat (getenv "JIRA_USER_EMAIL") ":" (getenv "JIRA_API_TOKEN"))
           t)))

;; --- Tabulated-list mode ----------------------------------------------------

(defvar jira-issues-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")   #'jira--open-issue-at-point)
    (define-key map (kbd "o")     #'jira--open-issue-at-point)
    (define-key map [mouse-1]     #'jira--open-issue-at-point)
    (define-key map [follow-link] 'mouse-face)
    map))

(define-derived-mode jira-issues-list-mode tabulated-list-mode "JIRA Issues"
  "Major mode for browsing a list of JIRA issues.
\\{jira-issues-list-mode-map}"
  (setq tabulated-list-format
        [("Key"      14 t)
         ("Created"  12 t)
         ("Status"   16 t)
         ("Priority"  9 t)
         ("Summary"   0 nil)])
  (tabulated-list-init-header))

(defun jira--open-issue-at-point ()
  "Open the JIRA issue at point in a web browser."
  (interactive)
  (if-let* ((url (tabulated-list-get-id)))
      (browse-url url)
    (message "No issue at point")))

;; --- Display ----------------------------------------------------------------

(defun jira--issue-to-entry (issue jira-base-url)
  "Convert an ISSUE alist into a `tabulated-list-mode' entry."
  (let* ((key     (cdr (assoc 'key issue)))
         (fields  (cdr (assoc 'fields issue)))
         (summary (or (cdr (assoc 'summary fields)) ""))
         (status  (or (cdr (assoc 'name (cdr (assoc 'status fields)))) ""))
         (created (or (cdr (assoc 'created fields)) ""))
         (date    (if (>= (length created) 10) (substring created 0 10) created))
         (prio    (or (cdr (assoc 'name (cdr (assoc 'priority fields)))) ""))
         (url     (concat (string-trim-right jira-base-url "/") "/browse/" key)))
    (list url (vector (propertize key
                                  'mouse-face  'highlight
                                  'help-echo   (concat "mouse-1: open in browser\n" url)
                                  'follow-link t)
                      date status prio summary))))

(defun jira--display-issues (issues jira-base-url)
  "Render ISSUES in the filed-issues buffer and switch to it."
  (let ((buf (get-buffer-create jira--buffer-name)))
    (with-current-buffer buf
      (jira-issues-list-mode)
      (setq tabulated-list-entries
            (mapcar (lambda (i) (jira--issue-to-entry i jira-base-url)) issues))
      (tabulated-list-print t)
      (goto-char (point-min)))
    (switch-to-buffer-other-window buf)))

;; --- Paginated fetch --------------------------------------------------------

(defun jira--fetch-page (base next-page-token accumulated)
  "Fetch one page of issues, appending to ACCUMULATED.
NEXT-PAGE-TOKEN is nil for the first request, or the cursor string from
the previous response.  When all pages arrive, render the buffer."
  (let* ((url       (concat base "/rest/api/3/search/jql"))
         (body-alist `((jql        . "reporter = currentUser() ORDER BY created DESC")
                       (maxResults . ,jira--page-size)
                       (fields     . ["summary" "status" "created" "priority"])))
         (body-alist (if next-page-token
                         (append body-alist `((nextPageToken . ,next-page-token)))
                       body-alist))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(jira--auth-header))
            ("Content-Type"  . "application/json")
            ("Accept"        . "application/json")))
         (url-request-data (encode-coding-string (json-encode body-alist) 'utf-8)))
    (url-retrieve url (lambda (status)
                        (jira--page-callback status base accumulated)))))

(defun jira--page-callback (status base accumulated)
  "Handle one page of JIRA search results and fetch the next if needed."
  (if (plist-get status :error)
      (message "JIRA request failed: %s" (plist-get status :error))
    (goto-char (point-min))
    (when (re-search-forward "\n\r?\n\r?" nil t)
      (delete-region (point-min) (point)))
    (condition-case err
        (let* ((json-object-type 'alist)
               (json-array-type  'list)
               (data        (json-read-from-string (buffer-string)))
               (page        (cdr (assoc 'issues data)))
               (next-token  (cdr (assoc 'nextPageToken data)))
               (all-issues  (append accumulated page)))
          (kill-buffer (current-buffer))
          (message "Fetching your filed JIRA issues... %d so far" (length all-issues))
          (if next-token
              (jira--fetch-page base next-token all-issues)
            (if all-issues
                (jira--display-issues all-issues base)
              (message "No filed JIRA issues found."))))
      (error (message "Failed to parse JIRA response: %s" err)))))

;; --- Entry point ------------------------------------------------------------

(defun show-all-my-filed-jira-issues ()
  "List all JIRA issues you filed, newest first.

Required environment variables:
  JIRAURL         - Root URL of the JIRA instance.
  JIRA_USER_EMAIL - Your Atlassian account email.
  JIRA_API_TOKEN  - Atlassian API token (https://id.atlassian.com/manage-profile/security/api-tokens).

In the results buffer press RET, o, or click to open the issue in a browser."
  (interactive)
  (unless (getenv "JIRAURL")
    (user-error "JIRAURL environment variable is not set"))
  (unless (getenv "JIRA_USER_EMAIL")
    (user-error "JIRA_USER_EMAIL environment variable is not set"))
  (unless (getenv "JIRA_API_TOKEN")
    (user-error "JIRA_API_TOKEN environment variable is not set"))
  (let ((base (string-trim-right (getenv "JIRAURL") "/")))
    (message "Fetching your filed JIRA issues...")
    (jira--fetch-page base nil nil)))

;; --- Navigate to JIRA issue (background autocomplete) ----------------------

(defvar jira--nav-cache nil
  "Alist of (key . (summary . browse-url)) populated by background fetch.")

(defvar jira--nav-fetch-active nil
  "Non-nil while a background navigation cache fetch is in progress.")

(defun jira--nav-fetch-page (base next-token)
  "Fetch one page of issues into `jira--nav-cache', then continue with NEXT-TOKEN."
  (let* ((url        (concat base "/rest/api/3/search/jql"))
         (body-alist `((jql        . "reporter = currentUser() ORDER BY created DESC")
                       (maxResults . ,jira--page-size)
                       (fields     . ["summary"])))
         (body-alist (if next-token
                         (append body-alist `((nextPageToken . ,next-token)))
                       body-alist))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(jira--auth-header))
            ("Content-Type"  . "application/json")
            ("Accept"        . "application/json")))
         (url-request-data (encode-coding-string (json-encode body-alist) 'utf-8)))
    (url-retrieve url (lambda (status)
                        (jira--nav-page-callback status base)))))

(defun jira--nav-page-callback (status base)
  "Append one page of issues to `jira--nav-cache' and continue paginating."
  (if (plist-get status :error)
      (progn
        (setq jira--nav-fetch-active nil)
        (message "JIRA nav fetch failed: %s" (plist-get status :error)))
    (goto-char (point-min))
    (when (re-search-forward "\n\r?\n\r?" nil t)
      (delete-region (point-min) (point)))
    (condition-case err
        (let* ((json-object-type 'alist)
               (json-array-type  'list)
               (data        (json-read-from-string (buffer-string)))
               (page        (cdr (assoc 'issues data)))
               (next-token  (cdr (assoc 'nextPageToken data))))
          (kill-buffer (current-buffer))
          (dolist (issue page)
            (let* ((key     (cdr (assoc 'key issue)))
                   (summary (or (cdr (assoc 'summary (cdr (assoc 'fields issue)))) ""))
                   (url     (concat base "/browse/" key)))
              (unless (assoc key jira--nav-cache)
                (push (cons key (cons summary url)) jira--nav-cache))))
          (if next-token
              (jira--nav-fetch-page base next-token)
            (setq jira--nav-fetch-active nil)
            (message "JIRA: %d issues loaded." (length jira--nav-cache))))
      (error
       (setq jira--nav-fetch-active nil)
       (message "JIRA nav fetch error: %s" err)))))

(defun navigate-to-jira-issue ()
  "Navigate to a JIRA issue selected via completion.
Issues are fetched in the background; more candidates appear as pages arrive.
Each candidate shows the issue key with its summary as an annotation.
Selecting an issue opens it in the browser.

Call with a prefix argument (C-u) to refresh the cache."
  (interactive)
  (unless (getenv "JIRAURL")
    (user-error "JIRAURL environment variable is not set"))
  (unless (getenv "JIRA_USER_EMAIL")
    (user-error "JIRA_USER_EMAIL environment variable is not set"))
  (unless (getenv "JIRA_API_TOKEN")
    (user-error "JIRA_API_TOKEN environment variable is not set"))
  (let ((base (string-trim-right (getenv "JIRAURL") "/")))
    (when (or current-prefix-arg
              (and (null jira--nav-cache) (null jira--nav-fetch-active)))
      (setq jira--nav-cache        nil
            jira--nav-fetch-active t)
      (jira--nav-fetch-page base nil))
    (let* ((annotate  (lambda (key)
                        (let ((entry (cdr (assoc key jira--nav-cache))))
                          (when entry
                            (concat "  " (car entry))))))
           (table     (lambda (str pred action)
                        (if (eq action 'metadata)
                            `(metadata
                              (category            . jira-issue)
                              (annotation-function . ,annotate))
                          (complete-with-action
                           action (mapcar #'car jira--nav-cache) str pred))))
           (prompt    (if jira--nav-fetch-active
                          "JIRA Issue (loading…): "
                        "JIRA Issue: "))
           (choice    (completing-read prompt table nil nil)))
      (when (and choice (not (string-empty-p choice)))
        (let* ((entry (cdr (assoc choice jira--nav-cache)))
               (url   (if entry (cdr entry) (concat base "/browse/" choice))))
          (browse-url url))))))

;; --- Search JIRA issues by title -------------------------------------------

(defun jira--nav-ensure-cache (base)
  "Start background cache fetch for BASE if not already loaded or loading."
  (when (and (null jira--nav-cache) (null jira--nav-fetch-active))
    (setq jira--nav-fetch-active t)
    (jira--nav-fetch-page base nil)))

(defun jira--search-candidates ()
  "Return a list of \"KEY  summary\" strings from `jira--nav-cache'."
  (mapcar (lambda (entry)
            (format "%-16s %s" (car entry) (cadr entry)))
          jira--nav-cache))

(defun jira--search-url (choice)
  "Return the browse URL for a CHOICE string produced by `jira--search-candidates'."
  (let ((key (car (split-string (string-trim choice)))))
    (or (cddr (assoc key jira--nav-cache))
        (concat (string-trim-right (getenv "JIRAURL") "/") "/browse/" key))))

(defun search-for-jira-issue ()
  "Search your filed JIRA issues by title, with regex support.
Candidates show \"KEY  summary\"; type any substring or regex to filter across
both fields.  Selecting an entry opens it in the browser.

Uses the same background cache as `navigate-to-jira-issue'; call with a
prefix argument (C-u) to force a cache refresh."
  (interactive)
  (unless (getenv "JIRAURL")
    (user-error "JIRAURL environment variable is not set"))
  (unless (getenv "JIRA_USER_EMAIL")
    (user-error "JIRA_USER_EMAIL environment variable is not set"))
  (unless (getenv "JIRA_API_TOKEN")
    (user-error "JIRA_API_TOKEN environment variable is not set"))
  (let ((base (string-trim-right (getenv "JIRAURL") "/")))
    (when current-prefix-arg
      (setq jira--nav-cache nil jira--nav-fetch-active nil))
    (jira--nav-ensure-cache base)
    (let* ((table  (lambda (str pred action)
                     (if (eq action 'metadata)
                         '(metadata (category . jira-issue))
                       (complete-with-action action (jira--search-candidates) str pred))))
           (prompt (if jira--nav-fetch-active
                       "Search JIRA titles (loading…): "
                     "Search JIRA titles: "))
           (completion-styles '(substring regexp basic))
           (choice (completing-read prompt table nil nil)))
      (when (and choice (not (string-empty-p choice)))
        (browse-url (jira--search-url choice))))))

(provide 'jira)
