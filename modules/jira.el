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

(require 'cl-lib)
(require 'crm)
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

(defvar jira-issues-list-mode-map (make-sparse-keymap))
(define-key jira-issues-list-mode-map (kbd "RET")   #'jira--open-issue-at-point)
(define-key jira-issues-list-mode-map (kbd "o")     #'jira--open-issue-at-point)
(define-key jira-issues-list-mode-map (kbd "s")     #'jira--set-status-at-point)
(define-key jira-issues-list-mode-map (kbd "p")     #'jira--set-priority-at-point)
(define-key jira-issues-list-mode-map [mouse-1]     #'jira--mouse-1)
(define-key jira-issues-list-mode-map [follow-link] 'mouse-face)

(define-derived-mode jira-issues-list-mode tabulated-list-mode "JIRA Issues"
  "Major mode for browsing a list of JIRA issues.
\\{jira-issues-list-mode-map}"
  (setq tabulated-list-format
        [("Key"      14 t)
         ("Created"  12 t)
         ("Updated"  12 t)
         ("Status"   14 t)
         ("Priority" 12 t)
         ("Summary"   0 t)])
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
         (updated (or (cdr (assoc 'updated fields)) ""))
         (date    (if (>= (length created) 10) (substring created 0 10) created))
         (udate   (if (>= (length updated) 10) (substring updated 0 10) updated))
         (prio    (or (cdr (assoc 'name (cdr (assoc 'priority fields)))) ""))
         (url     (concat (string-trim-right jira-base-url "/") "/browse/" key)))
    (list url (vector
               (propertize key
                           'mouse-face  'highlight
                           'help-echo   (concat "mouse-1: open in browser\n" url)
                           'follow-link t)
               date
               udate
               (propertize status
                           'jira-editable-field "status"
                           'jira-issue-key      key
                           'mouse-face          'highlight
                           'help-echo           "mouse-1 or s: change status")
               (propertize prio
                           'jira-editable-field "priority"
                           'jira-issue-key      key
                           'mouse-face          'highlight
                           'help-echo           "mouse-1 or p: change priority")
               summary))))

(defvar-local jira--list-base nil
  "Base URL of the JIRA instance for this issues list buffer.")

(defun jira--display-issues (issues jira-base-url buffer-name)
  "Render ISSUES in BUFFER-NAME and switch to it."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (jira-issues-list-mode)
      (setq-local jira--list-base jira-base-url)
      (setq tabulated-list-entries
            (mapcar (lambda (i) (jira--issue-to-entry i jira-base-url)) issues))
      (tabulated-list-print t)
      (goto-char (point-min)))
    (switch-to-buffer-other-window buf)))

;; --- Edit fields in-place ---------------------------------------------------

(defun jira--key-from-url (url)
  "Extract the JIRA issue key from a browse URL like .../browse/KEY-123."
  (when (and url (string-match "/browse/\\([A-Z][A-Z0-9_]+-[0-9]+\\)" url))
    (match-string 1 url)))

(defun jira--fetch-priorities-sync (base)
  "Return a list of priority name strings from the JIRA instance at BASE."
  (mapcar (lambda (p) (cdr (assoc 'name p)))
          (jira--fetch-json-sync (concat base "/rest/api/3/priority"))))

(defun jira--fetch-transitions-sync (base key)
  "Return an alist of (NAME . ID) for available status transitions on issue KEY."
  (let* ((url  (concat base "/rest/api/3/issue/" (url-hexify-string key) "/transitions"))
         (data (jira--fetch-json-sync url))
         (trs  (cdr (assoc 'transitions data))))
    (mapcar (lambda (tr) (cons (cdr (assoc 'name tr)) (cdr (assoc 'id tr)))) trs)))

(defun jira--update-entry-in-buffer (issue-url col-idx new-value)
  "Update column COL-IDX to NEW-VALUE for the entry at ISSUE-URL, then reprint."
  (setq tabulated-list-entries
        (mapcar (lambda (entry)
                  (if (string= (car entry) issue-url)
                      (let ((vec (copy-sequence (cadr entry))))
                        (aset vec col-idx new-value)
                        (list (car entry) vec))
                    entry))
                tabulated-list-entries))
  (tabulated-list-print t))

(defun jira--put-issue-field (base key fields-alist callback)
  "Async PUT FIELDS-ALIST to JIRA issue KEY at BASE; call CALLBACK on success."
  (let* ((url-request-method "PUT")
         (url-request-extra-headers
          `(("Authorization" . ,(jira--auth-header))
            ("Content-Type"  . "application/json")
            ("Accept"        . "application/json")))
         (url-request-data
          (encode-coding-string (json-encode `((fields . ,fields-alist))) 'utf-8)))
    (url-retrieve
     (concat base "/rest/api/3/issue/" (url-hexify-string key))
     (lambda (status)
       (if (plist-get status :error)
           (message "JIRA: update failed: %s" (plist-get status :error))
         (kill-buffer (current-buffer))
         (funcall callback))))))

(defun jira--post-transition (base key transition-id callback)
  "Async POST transition TRANSITION-ID for JIRA issue KEY at BASE."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(jira--auth-header))
            ("Content-Type"  . "application/json")
            ("Accept"        . "application/json")))
         (url-request-data
          (encode-coding-string
           (json-encode `((transition . ((id . ,transition-id))))) 'utf-8)))
    (url-retrieve
     (concat base "/rest/api/3/issue/" (url-hexify-string key) "/transitions")
     (lambda (status)
       (if (plist-get status :error)
           (message "JIRA: transition failed: %s" (plist-get status :error))
         (kill-buffer (current-buffer))
         (funcall callback))))))

(defun jira--set-status-at-point ()
  "Prompt to change the JIRA status of the issue on the current line."
  (interactive)
  (let* ((issue-url (tabulated-list-get-id))
         (key       (jira--key-from-url issue-url))
         (base      jira--list-base)
         (buf       (current-buffer)))
    (unless key (user-error "No JIRA issue at point"))
    (message "Fetching available transitions for %s..." key)
    (let* ((transitions (jira--fetch-transitions-sync base key)))
      (unless transitions (user-error "No transitions available for %s" key))
      (let* ((names  (mapcar #'car transitions))
             (chosen (completing-read (format "New status for %s: " key) names nil t))
             (tid    (cdr (assoc chosen transitions))))
        (unless tid (user-error "Unknown transition: %s" chosen))
        (message "Setting status of %s to %s..." key chosen)
        (jira--post-transition
         base key tid
         (lambda ()
           (message "JIRA: %s status → %s" key chosen)
           (with-current-buffer buf
             (jira--update-entry-in-buffer
              issue-url 3
              (propertize chosen
                          'jira-editable-field "status"
                          'jira-issue-key      key
                          'mouse-face          'highlight
                          'help-echo           "mouse-1 or s: change status")))))))))

(defun jira--set-priority-at-point ()
  "Prompt to change the JIRA priority of the issue on the current line."
  (interactive)
  (let* ((issue-url (tabulated-list-get-id))
         (key       (jira--key-from-url issue-url))
         (base      jira--list-base)
         (buf       (current-buffer)))
    (unless key (user-error "No JIRA issue at point"))
    (let* ((priorities (or (jira--fetch-priorities-sync base)
                           '("Highest" "High" "Medium" "Low" "Lowest")))
           (chosen     (completing-read (format "New priority for %s: " key) priorities nil t)))
      (message "Setting priority of %s to %s..." key chosen)
      (jira--put-issue-field
       base key `((priority . ((name . ,chosen))))
       (lambda ()
         (message "JIRA: %s priority → %s" key chosen)
         (with-current-buffer buf
           (jira--update-entry-in-buffer
            issue-url 4
            (propertize chosen
                        'jira-editable-field "priority"
                        'jira-issue-key      key
                        'mouse-face          'highlight
                        'help-echo           "mouse-1 or p: change priority"))))))))

(defun jira--mouse-1 (event)
  "Open the JIRA issue in browser, or edit Status/Priority when clicking those columns."
  (interactive "e")
  (let* ((pos   (posn-point (event-end event)))
         (field (get-text-property pos 'jira-editable-field)))
    (cond
     ((equal field "status")
      (goto-char pos)
      (jira--set-status-at-point))
     ((equal field "priority")
      (goto-char pos)
      (jira--set-priority-at-point))
     (t
      (mouse-set-point event)
      (jira--open-issue-at-point)))))

;; --- Paginated fetch --------------------------------------------------------

(defun jira--fetch-page (base jql buffer-name next-page-token accumulated)
  "Fetch one page of issues for JQL, appending to ACCUMULATED.
NEXT-PAGE-TOKEN is nil for the first request, or the cursor string from
the previous response.  When all pages arrive, render BUFFER-NAME."
  (let* ((url       (concat base "/rest/api/3/search/jql"))
         (body-alist `((jql        . ,jql)
                       (maxResults . ,jira--page-size)
                       (fields     . ["summary" "status" "created" "updated" "priority"])))
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
                        (jira--page-callback status base jql buffer-name accumulated)))))

(defun jira--page-callback (status base jql buffer-name accumulated)
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
          (message "Fetching JIRA issues... %d so far" (length all-issues))
          (if next-token
              (jira--fetch-page base jql buffer-name next-token all-issues)
            (if all-issues
                (jira--display-issues all-issues base buffer-name)
              (message "No JIRA issues found."))))
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
    (jira--fetch-page base "reporter = currentUser() ORDER BY created DESC"
                      jira--buffer-name nil nil)))

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
  "Search your filed JIRA issues by title.
Candidates show \"KEY  summary\"; type any substring to filter across
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
           (completion-styles '(substring basic))
           (choice (completing-read prompt table nil nil)))
      (when (and choice (not (string-empty-p choice)))
        (browse-url (jira--search-url choice))))))

;; --- New issue form ----------------------------------------------------------

(defvar-local jira--form-base nil)
(defvar-local jira--form-project-key nil)
(defvar-local jira--form-issuetype nil)
(defvar-local jira--form-priority nil)
(defvar-local jira--form-assignee-id nil)
(defvar-local jira--form-reporter-id nil)
(defvar-local jira--form-components nil)
(defvar-local jira--form-labels nil)

(defvar jira-new-issue-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'jira--submit-new-issue)
    map))

(define-derived-mode jira-new-issue-mode text-mode "JIRA New Issue"
  "Major mode for composing and filing a new JIRA issue.
\\{jira-new-issue-mode-map}"
  (setq-local font-lock-defaults
              '((("^#.*"        0 font-lock-comment-face)
                 ("^[A-Za-z]+:" 0 font-lock-keyword-face))))
  (require 'whitespace)
  (setq-local whitespace-style
              '(face tabs spaces trailing tab-mark space-mark))
  (whitespace-mode 1))

(defun jira--fetch-json-sync (url)
  "Fetch URL with JIRA auth and return parsed JSON, or nil on error."
  (let* ((url-request-extra-headers
          `(("Authorization" . ,(jira--auth-header))
            ("Accept"        . "application/json")))
         (buf (url-retrieve-synchronously url t)))
    (when buf
      (with-current-buffer buf
        (goto-char (point-min))
        (when (re-search-forward "\n\r?\n\r?" nil t)
          (delete-region (point-min) (point)))
        (condition-case nil
            (let* ((json-object-type 'alist)
                   (json-array-type  'list)
                   (data (json-read-from-string (buffer-string))))
              (kill-buffer buf)
              data)
          (error (kill-buffer buf) nil))))))

(defun jira--fetch-projects-sync (base)
  "Return an alist of (KEY . NAME) for all accessible JIRA projects at BASE."
  (mapcar (lambda (p) (cons (cdr (assoc 'key p)) (cdr (assoc 'name p))))
          (jira--fetch-json-sync (concat base "/rest/api/3/project"))))

(defun jira--fetch-issue-types-sync (base project-key)
  "Return a list of issue type name strings for PROJECT-KEY."
  (let* ((url   (concat base "/rest/api/3/issue/createmeta/"
                        (url-hexify-string project-key) "/issuetypes"))
         (data  (jira--fetch-json-sync url))
         (types (cdr (assoc 'issueTypes data))))
    (mapcar (lambda (itype) (cdr (assoc 'name itype))) types)))

(defun jira--fetch-components-sync (base project-key)
  "Return a list of component name strings for PROJECT-KEY."
  (let* ((url  (concat base "/rest/api/3/project/"
                       (url-hexify-string project-key) "/components"))
         (data (jira--fetch-json-sync url)))
    (mapcar (lambda (c) (cdr (assoc 'name c))) data)))

(defun jira--fetch-assignable-users-by-query (base project-key query)
  "Return alist of (DISPLAY-STRING . ACCOUNT-ID) for assignable users matching QUERY in PROJECT-KEY.
QUERY is sent as the `query' parameter to JIRA's user/assignable/search endpoint;
without it JIRA Cloud often returns an incomplete subset."
  (let* ((url  (concat base "/rest/api/3/user/assignable/search"
                       "?project=" (url-hexify-string project-key)
                       "&query="   (url-hexify-string (or query ""))
                       "&maxResults=50"))
         (data (jira--fetch-json-sync url)))
    (mapcar (lambda (u)
              (let* ((name  (or (cdr (assoc 'displayName  u)) ""))
                     (email (or (cdr (assoc 'emailAddress u)) ""))
                     (aid   (cdr (assoc 'accountId u)))
                     (label (if (string-empty-p email)
                                (format "%s · %s" name (or aid ""))
                              (format "%s (%s)" name email))))
                (cons label aid)))
            (or data nil))))

(defun jira--read-jira-user (prompt base project-key &optional initial-input)
  "Prompt for a JIRA user with live completion against the assignable-users endpoint.
PROMPT is the minibuffer prompt; INITIAL-INPUT pre-fills the search query.
`(unassigned)' is always offered as a candidate and returned with a nil account id.
Returns (DISPLAY-STRING . ACCOUNT-ID)."
  (let* ((query-cache (make-hash-table :test 'equal))
         (label->id   (make-hash-table :test 'equal))
         (table
          (lambda (string pred action)
            (let* ((q (string-trim string))
                   (entries
                    (cond
                     ((< (length q) 2) nil)
                     (t (or (gethash q query-cache)
                            (let ((fresh (jira--fetch-assignable-users-by-query
                                          base project-key q)))
                              (puthash q fresh query-cache)
                              (dolist (e fresh)
                                (puthash (car e) (cdr e) label->id))
                              fresh)))))
                   (candidates (cons "(unassigned)" (mapcar #'car entries))))
              (cond
               ((eq action 'metadata)
                '(metadata (category . jira-user)))
               ((eq (car-safe action) 'boundaries) nil)
               (t (complete-with-action action candidates string pred))))))
         (chosen (completing-read prompt table nil t initial-input nil "(unassigned)")))
    (if (equal chosen "(unassigned)")
        (cons chosen nil)
      (cons chosen (gethash chosen label->id)))))

(defun jira--read-assignee (base project-key)
  "Prompt for an assignee with live completion against JIRA.
Returns (DISPLAY-STRING . ACCOUNT-ID); ACCOUNT-ID is nil for unassigned."
  (jira--read-jira-user "Assignee (type ≥2 chars to search): " base project-key))

(defun jira--read-reporter (base project-key)
  "Prompt for a reporter with live completion against JIRA.
Pre-fills the search box with $P4USER so the matching JIRA user can be picked
quickly.  Returns (DISPLAY-STRING . ACCOUNT-ID); ACCOUNT-ID is nil for the
default (the API caller)."
  (jira--read-jira-user "Reporter (type ≥2 chars to search): " base project-key
                        (or (getenv "P4USER") "")))

(defun jira--fetch-labels-sync (base)
  "Return a list of label strings from the JIRA instance at BASE (first 1000)."
  (let* ((url  (concat base "/rest/api/3/label?maxResults=1000"))
         (data (jira--fetch-json-sync url)))
    (cdr (assoc 'values data))))

(defun jira--new-issue-template (proj-key proj-name issuetype priority
                                assignee-display reporter-display components labels)
  "Return the initial content string for a new-issue form buffer."
  (concat
   (format "# New JIRA issue — %s (%s)\n" proj-name proj-key)
   "# C-c C-c to submit · C-x k to cancel\n"
   "#\n"
   (format "# Project:    %s\n" proj-key)
   (format "# IssueType:  %s\n" issuetype)
   (format "# Priority:   %s\n" priority)
   (format "# Assignee:   %s\n"
           (if (string-empty-p (or assignee-display "")) "(unassigned)" assignee-display))
   (format "# Reporter:   %s\n"
           (if (string-empty-p (or reporter-display "")) "(default)" reporter-display))
   (format "# Components: %s\n" (if components (string-join components ", ") "(none)"))
   (format "# Labels:     %s\n" (if labels (string-join labels ", ") "(none)"))
   "#\n"
   "Summary:     \n"
   "\n"
   "Description:\n"
   "    \n"))

(defun jira--parse-form-buffer ()
  "Parse the current new-issue buffer into an alist of (FIELD . VALUE)."
  (let (fields current-key lines)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ((string-match-p "^#" line))
           ((string-match "^\\([A-Za-z]+\\):\\(.*\\)$" line)
            (when current-key
              (push (cons current-key
                          (string-trim (mapconcat #'identity (nreverse lines) "\n")))
                    fields)
              (setq current-key nil lines nil))
            (let* ((key (match-string 1 line))
                   (val (string-trim (match-string 2 line))))
              (if (string-empty-p val)
                  (setq current-key key lines nil)
                (push (cons key val) fields))))
           ((and current-key (string-match "^[ \t]+\\(.*\\)$" line))
            (push (match-string 1 line) lines))
           (current-key
            (push "" lines))))
        (forward-line 1))
      (when current-key
        (push (cons current-key
                    (string-trim (mapconcat #'identity (nreverse lines) "\n")))
              fields)))
    (nreverse fields)))

(defun jira--text-to-adf (text)
  "Convert plain TEXT to an Atlassian Document Format (ADF) alist."
  `((type    . "doc")
    (version . 1)
    (content . ,(vconcat
                 (mapcar (lambda (para)
                           `((type    . "paragraph")
                             (content . [((type . "text")
                                          (text . ,para))])))
                         (split-string (string-trim text) "\n\n+" t))))))

(defun jira--build-issue-body (proj-key parsed)
  "Return the JSON body alist for POST /rest/api/3/issue."
  (let* ((get    (lambda (k) (string-trim (or (cdr (assoc k parsed)) ""))))
         (fields `((project   . ((key  . ,proj-key)))
                   (issuetype . ((name . ,jira--form-issuetype)))
                   (summary   . ,(funcall get "Summary")))))
    (when (and jira--form-priority (not (string-empty-p jira--form-priority)))
      (push `(priority . ((name . ,jira--form-priority))) fields))
    (when jira--form-assignee-id
      (push `(assignee . ((accountId . ,jira--form-assignee-id))) fields))
    (when jira--form-reporter-id
      (push `(reporter . ((accountId . ,jira--form-reporter-id))) fields))
    (when jira--form-components
      (push `(components . ,(vconcat (mapcar (lambda (c) `((name . ,c)))
                                             jira--form-components)))
            fields))
    (when jira--form-labels
      (push `(labels . ,(vconcat jira--form-labels)) fields))
    (let ((desc (funcall get "Description")))
      (unless (string-empty-p desc)
        (push `(description . ,(jira--text-to-adf desc)) fields)))
    `((fields . ,fields))))

(defun jira--parse-error-body (body)
  "Extract a readable message from a JIRA error response BODY string."
  (condition-case nil
      (let* ((json-object-type 'alist)
             (json-array-type  'list)
             (data (json-read-from-string body))
             (msgs (cdr (assoc 'errorMessages data)))
             (errs (cdr (assoc 'errors data)))
             (parts nil))
        (when msgs
          (push (mapconcat #'identity msgs " | ") parts))
        (when errs
          (dolist (e errs)
            (push (format "%s: %s" (car e) (cdr e)) parts)))
        (if parts
            (mapconcat #'identity (nreverse parts) " | ")
          body))
    (error body)))

(defun jira--submit-new-issue ()
  "Parse the current form buffer and POST the new issue to JIRA."
  (interactive)
  (let* ((parsed   (jira--parse-form-buffer))
         (summary  (string-trim (or (cdr (assoc "Summary" parsed)) "")))
         (base     jira--form-base)
         (proj-key jira--form-project-key))
    (when (string-empty-p summary)
      (user-error "Summary is required"))
    (let* ((body-alist (jira--build-issue-body proj-key parsed))
           (json-body  (json-encode body-alist))
           (url-request-method "POST")
           (url-request-extra-headers
            `(("Authorization" . ,(jira--auth-header))
              ("Content-Type"  . "application/json")
              ("Accept"        . "application/json")))
           (url-request-data (encode-coding-string json-body 'utf-8)))
      (message "JIRA: Submitting issue (body logged to *Messages*)...")
      (message "JIRA request body: %s" json-body)
      (url-retrieve
       (concat base "/rest/api/3/issue")
       (lambda (status)
         (goto-char (point-min))
         (when (re-search-forward "\n\r?\n\r?" nil t)
           (delete-region (point-min) (point)))
         (let ((body (buffer-string)))
           (kill-buffer (current-buffer))
           (if (plist-get status :error)
               (message "JIRA error: %s" (jira--parse-error-body body))
             (condition-case err
                 (let* ((json-object-type 'alist)
                        (json-array-type  'list)
                        (data (json-read-from-string body))
                        (key  (cdr (assoc 'key data))))
                   (if key
                       (let ((issue-url (concat base "/browse/" key)))
                         (message "JIRA: Created %s — %s" key issue-url)
                         (when (y-or-n-p (format "Issue %s created. Open in browser? " key))
                           (browse-url issue-url)))
                     (message "JIRA: issue created but could not parse key from response")))
               (error (message "JIRA: error parsing response: %s" err))))))))))

(defun file-new-jira-issue ()
  "Compose and file a new JIRA issue interactively.
Prompts for project, issue type, priority, assignee, components, and labels
with completion before opening a form buffer for summary and description.
Press C-c C-c to submit, or C-x k to cancel."
  (interactive)
  (unless (getenv "JIRAURL")
    (user-error "JIRAURL environment variable is not set"))
  (unless (getenv "JIRA_USER_EMAIL")
    (user-error "JIRA_USER_EMAIL environment variable is not set"))
  (unless (getenv "JIRA_API_TOKEN")
    (user-error "JIRA_API_TOKEN environment variable is not set"))
  (let* ((base (string-trim-right (getenv "JIRAURL") "/")))
    (message "Fetching JIRA projects...")
    (let* ((projects (jira--fetch-projects-sync base)))
      (unless projects
        (user-error "Could not fetch JIRA projects — check credentials and JIRAURL"))
      (let* ((proj-candidates (mapcar (lambda (p)
                                        (format "%-12s %s" (car p) (cdr p)))
                                      projects))
             (proj-chosen  (completing-read "Project: " proj-candidates nil t))
             (proj-key     (string-trim (car (split-string proj-chosen))))
             (proj-name    (or (cdr (assoc proj-key projects)) proj-key)))
        (message "Fetching metadata for %s..." proj-key)
        (let* ((types      (or (jira--fetch-issue-types-sync base proj-key)
                               '("Task" "Bug" "Story" "Epic")))
               (priorities (or (jira--fetch-priorities-sync base)
                               '("Highest" "High" "Medium" "Low" "Lowest")))
               (components (jira--fetch-components-sync base proj-key))
               (labels     (jira--fetch-labels-sync base)))
          ;; --- Pre-prompts with autocomplete ---
          (let* ((issuetype
                  (completing-read "IssueType: " types nil t nil nil (car types)))
                 (priority
                  (completing-read "Priority: " priorities nil t nil nil "Medium"))
                 (assignee-pair (jira--read-assignee base proj-key))
                 (assignee-str  (car assignee-pair))
                 (assignee-id   (cdr assignee-pair))
                 (reporter-pair (jira--read-reporter base proj-key))
                 (reporter-str  (car reporter-pair))
                 (reporter-id   (cdr reporter-pair))
                 (chosen-comps
                  (when components
                    (cl-remove-if #'string-empty-p
                                  (mapcar #'string-trim
                                          (completing-read-multiple "Components (RET to skip): "
                                                                    components nil nil)))))
                 (chosen-labels
                  (cl-remove-if #'string-empty-p
                                 (mapcar #'string-trim
                                         (completing-read-multiple "Labels (RET to skip): "
                                                                   labels nil nil))))
                 (buf-name (format "*JIRA: New Issue — %s*" proj-key))
                 (buf      (get-buffer-create buf-name)))
            (with-current-buffer buf
              (jira-new-issue-mode)
              (setq-local jira--form-base         base)
              (setq-local jira--form-project-key  proj-key)
              (setq-local jira--form-issuetype    issuetype)
              (setq-local jira--form-priority     priority)
              (setq-local jira--form-assignee-id  assignee-id)
              (setq-local jira--form-reporter-id  reporter-id)
              (setq-local jira--form-components   chosen-comps)
              (setq-local jira--form-labels       chosen-labels)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (jira--new-issue-template proj-key proj-name issuetype priority
                                                 assignee-str reporter-str
                                                 chosen-comps chosen-labels))))
            (switch-to-buffer buf)
            (goto-char (point-min))
            (when (re-search-forward "^Summary:[ \t]*" nil t)
              (goto-char (match-end 0)))))))))


;; --- Status-filtered issue list --------------------------------------------

(defun jira--fetch-statuses-sync (base)
  "Return a list of status name strings from the JIRA instance at BASE.
Returns nil if the request fails or cannot be parsed."
  (let* ((url (concat base "/rest/api/3/status"))
         (url-request-extra-headers
          `(("Authorization" . ,(jira--auth-header))
            ("Accept"        . "application/json")))
         (buf (url-retrieve-synchronously url t)))
    (when buf
      (with-current-buffer buf
        (goto-char (point-min))
        (when (re-search-forward "\n\r?\n\r?" nil t)
          (delete-region (point-min) (point)))
        (condition-case nil
            (let* ((json-object-type 'alist)
                   (json-array-type  'list)
                   (data (json-read-from-string (buffer-string))))
              (kill-buffer buf)
              (mapcar (lambda (s) (cdr (assoc 'name s))) data))
          (error (kill-buffer buf) nil))))))

(defun show-my-jira-issues-with-status ()
  "List JIRA issues you filed whose status matches your selection, newest first.
Prompts for one or more statuses with completion drawn from your JIRA instance.

Required environment variables:
  JIRAURL         - Root URL of the JIRA instance.
  JIRA_USER_EMAIL - Your Atlassian account email.
  JIRA_API_TOKEN  - Atlassian API token.

In the results buffer press RET, o, or click to open the issue in a browser."
  (interactive)
  (unless (getenv "JIRAURL")
    (user-error "JIRAURL environment variable is not set"))
  (unless (getenv "JIRA_USER_EMAIL")
    (user-error "JIRA_USER_EMAIL environment variable is not set"))
  (unless (getenv "JIRA_API_TOKEN")
    (user-error "JIRA_API_TOKEN environment variable is not set"))
  (let* ((base     (string-trim-right (getenv "JIRAURL") "/"))
         (statuses (or (jira--fetch-statuses-sync base)
                       '("To Do" "In Progress" "In Review" "Done" "Closed"
                         "Open" "Resolved" "Reopened" "Backlog")))
         (chosen   (mapcar #'string-trim
                           (completing-read-multiple "Status(es): " statuses nil nil))))
    (if (null chosen)
        (message "No statuses selected.")
      (let* ((status-jql (mapconcat (lambda (s) (format "\"%s\"" s)) chosen ", "))
             (jql        (format "reporter = currentUser() AND status in (%s) ORDER BY created DESC"
                                 status-jql))
             (buf-name   (format "*JIRA: Issues — %s*" (string-join chosen ", "))))
        (message "Fetching JIRA issues with status: %s..." (string-join chosen ", "))
        (jira--fetch-page base jql buf-name nil nil)))))

;; --- User- and status-filtered issue list -----------------------------------

(defun jira--fetch-users-by-query (base query)
  "Return an alist of (DISPLAY . ACCOUNT-ID) for users matching QUERY at BASE.
Uses the instance-wide user search, so no project context is needed.
App/customer accounts are filtered out."
  (let* ((url  (concat base "/rest/api/3/user/search"
                       "?query=" (url-hexify-string (or query ""))
                       "&maxResults=50"))
         (data (jira--fetch-json-sync url)))
    (delq nil
          (mapcar (lambda (u)
                    (let* ((name  (or (cdr (assoc 'displayName  u)) ""))
                           (email (or (cdr (assoc 'emailAddress u)) ""))
                           (aid   (cdr (assoc 'accountId u)))
                           (atype (or (cdr (assoc 'accountType u)) "atlassian")))
                      (when (and aid (string= atype "atlassian"))
                        (cons (if (string-empty-p email)
                                  (format "%s · %s" name aid)
                                (format "%s (%s)" name email))
                              aid))))
                  data))))

(defun jira--read-any-jira-user (prompt base)
  "Prompt with PROMPT for any JIRA user at BASE, with live completion.
Type at least two characters to search.  `(me)' is always offered and is
returned with a nil account id, meaning `currentUser()'.
Returns (DISPLAY-STRING . ACCOUNT-ID)."
  (let* ((query-cache (make-hash-table :test 'equal))
         (label->id   (make-hash-table :test 'equal))
         (table
          (lambda (string pred action)
            (let* ((q (string-trim string))
                   (entries
                    (when (>= (length q) 2)
                      (or (gethash q query-cache)
                          (let ((fresh (jira--fetch-users-by-query base q)))
                            (puthash q fresh query-cache)
                            (dolist (e fresh)
                              (puthash (car e) (cdr e) label->id))
                            fresh))))
                   (candidates (cons "(me)" (mapcar #'car entries))))
              (cond
               ((eq action 'metadata) '(metadata (category . jira-user)))
               ((eq (car-safe action) 'boundaries) nil)
               (t (complete-with-action action candidates string pred))))))
         (chosen (completing-read prompt table nil nil nil nil "(me)")))
    (if (or (null chosen) (string-empty-p chosen) (equal chosen "(me)"))
        (cons "(me)" nil)
      (cons chosen (gethash chosen label->id)))))

(defun jira--user-status-jql (field account-id statuses)
  "Build a JQL query matching ACCOUNT-ID in FIELD, restricted to STATUSES.
FIELD is \"Reporter\", \"Assignee\", or \"Either\".  A nil ACCOUNT-ID means
`currentUser()'.  A nil or empty STATUSES omits the status restriction."
  (let* ((who (if account-id (format "\"%s\"" account-id) "currentUser()"))
         (user-clause
          (cond
           ((equal field "Assignee") (format "assignee = %s" who))
           ((equal field "Either")   (format "(reporter = %s OR assignee = %s)" who who))
           (t                        (format "reporter = %s" who))))
         (status-clause
          (when statuses
            (format "status in (%s)"
                    (mapconcat (lambda (s) (format "\"%s\"" s)) statuses ", ")))))
    (concat user-clause
            (when status-clause (concat " AND " status-clause))
            " ORDER BY created DESC")))

(defun show-jira-issues-with-status ()
  "List JIRA issues for a chosen user and status(es), newest first.

Prompts, all with completion, for:
  User     - any JIRA user (type ≥2 characters to search); RET picks you.
  Field    - whether the user is the Reporter, the Assignee, or Either.
  Statuses - one or more comma-separated statuses; RET means any status.

Required environment variables:
  JIRAURL         - Root URL of the JIRA instance.
  JIRA_USER_EMAIL - Your Atlassian account email.
  JIRA_API_TOKEN  - Atlassian API token.

In the results buffer press RET, o, or click to open the issue in a browser;
press s to change status and p to change priority."
  (interactive)
  (unless (getenv "JIRAURL")
    (user-error "JIRAURL environment variable is not set"))
  (unless (getenv "JIRA_USER_EMAIL")
    (user-error "JIRA_USER_EMAIL environment variable is not set"))
  (unless (getenv "JIRA_API_TOKEN")
    (user-error "JIRA_API_TOKEN environment variable is not set"))
  (let* ((base       (string-trim-right (getenv "JIRAURL") "/"))
         (user-pair  (jira--read-any-jira-user "User (type ≥2 chars to search): " base))
         (user-str   (car user-pair))
         (account-id (cdr user-pair))
         (field      (completing-read "Match user as: "
                                      '("Reporter" "Assignee" "Either")
                                      nil t nil nil "Reporter"))
         (statuses   (or (jira--fetch-statuses-sync base)
                         '("To Do" "In Progress" "In Review" "Done" "Closed"
                           "Open" "Resolved" "Reopened" "Backlog")))
         (chosen     (cl-remove-if
                      #'string-empty-p
                      (mapcar #'string-trim
                              (completing-read-multiple
                               "Status(es) (RET for any): " statuses nil nil))))
         (short      (car (split-string user-str " (")))
         (jql        (jira--user-status-jql field account-id chosen))
         (buf-name   (format "*JIRA: %s %s — %s*"
                             short
                             (downcase field)
                             (if chosen (string-join chosen ", ") "any status"))))
    (message "Fetching JIRA issues: %s" jql)
    (jira--fetch-page base jql buf-name nil nil)))

(provide 'jira)
