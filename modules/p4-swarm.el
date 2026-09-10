;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'tabulated-list)
(require 'url)
(require 'p4)

;;; User options:

(defvar p4-swarm-api-version "v11"
  "Version of the Swarm REST API to talk to.")

(defvar p4-swarm-page-size 100
  "Number of records to request per Swarm API page.")

(defvar p4-swarm-max-pages 20
  "Maximum number of pages `p4-swarm-list-reviews' will fetch for one query.")

(defvar p4-swarm-default-max-reviews 50
  "Number of reviews `p4-swarm-list-reviews' shows when given no prefix argument.")

(defvar p4-swarm-timeout 30
  "Seconds to wait for a synchronous Swarm API request.")

(defvar p4-swarm-comment-context-lines 4
  "Number of preceding diff lines sent as the context of an inline comment.")

(defvar p4-swarm-roles '("participant" "author" "reviewer")
  "Roles `p4-swarm-list-reviews' can filter on.")

(defvar p4-swarm-states '("open" "closed" "all")
  "Review states `p4-swarm-list-reviews' can filter on.")

(defvar p4-swarm-state-filters
  '(("open"   . ("needsReview" "needsRevision" "approved:isPending"))
    ("closed" . ("approved:notPending" "approved:commit" "rejected" "archived"))
    ("all"    . nil))
  "Alist mapping a name in `p4-swarm-states' to Swarm `state[]' query values.")

(defvar p4-swarm-keywords-fields '("participants")
  "Value of the Swarm `keywordsFields[]' query parameter used to find reviews.
Swarm has no exact author or participant filter, so reviews are searched for by
keyword over this field and the exact role match is made locally by
`p4-swarm--matches-role-p'.  `participants' includes the author of a review.")

(defvar p4-swarm-review-fields
  '("id" "author" "participants" "participantsData" "description" "state"
    "stateLabel" "created" "updated" "pending" "commitStatus")
  "Review fields requested from Swarm, enough to fill `p4-swarm-list-mode'.")

;;; Configuration and authentication:

(defun p4-swarm--base-url ()
  "Return the root URL of the Swarm server, without a trailing slash."
  (let ((url (getenv "P4SWARMURL")))
    (when (or (null url) (string-empty-p url))
      (user-error "P4SWARMURL environment variable is not set"))
    (string-trim-right url "/")))

(defun p4-swarm--user ()
  "Return the Perforce user name requests are made as."
  (let ((user (getenv "P4USER")))
    (when (or (null user) (string-empty-p user))
      (user-error "P4USER environment variable is not set"))
    user))

(defun p4-swarm--auth-header ()
  "Return a Basic Authorization header value for Swarm API requests."
  (let ((token (getenv "P4SWARMTOKEN")))
    (when (or (null token) (string-empty-p token))
      (user-error
       "P4SWARMTOKEN is not set; create an API token on your Swarm profile page"))
    (concat "Basic "
            (base64-encode-string (concat (p4-swarm--user) ":" token) t))))

(defun p4-swarm--check-config ()
  "Signal a `user-error' unless Swarm is fully configured."
  (p4-swarm--base-url)
  (p4-swarm--auth-header)
  t)

;;; Requests:

(defun p4-swarm--encode-params (params)
  "Return PARAMS as a URL query string.
PARAMS is an alist of (NAME . VALUE).  A VALUE that is a list contributes one
NAME=VALUE pair per element; a NIL VALUE contributes nothing."
  (let (pairs)
    (dolist (param params)
      (let ((name (car param))
            (value (cdr param)))
        (dolist (v (if (and value (listp value)) value (and value (list value))))
          (push (concat (url-hexify-string name) "="
                        (url-hexify-string (format "%s" v)))
                pairs))))
    (mapconcat #'identity (nreverse pairs) "&")))

(defun p4-swarm--url (path &optional params)
  "Return the full Swarm API URL for PATH with query PARAMS."
  (let ((query (p4-swarm--encode-params params)))
    (concat (p4-swarm--base-url) "/api/" p4-swarm-api-version path
            (if (string-empty-p query) "" (concat "?" query)))))

(defun p4-swarm--parse-response ()
  "Parse the `url-retrieve' response in the current buffer.
Returns a cons of the HTTP status code (NIL when there was none) and the parsed
JSON body (NIL when the body is empty or unparseable)."
  (let ((code (and (boundp 'url-http-response-status) url-http-response-status))
        (body nil))
    (goto-char (point-min))
    (when (re-search-forward "\n\r?\n\r?" nil t)
      (setq body (buffer-substring-no-properties (point) (point-max))))
    (cons code
          (and body (not (string-blank-p body))
               (ignore-errors
                 (let ((json-object-type 'alist)
                       (json-array-type 'list)
                       (json-key-type 'symbol))
                   (json-read-from-string (decode-coding-string body 'utf-8))))))))

(defun p4-swarm--response-error (code payload)
  "Return an error string for a Swarm response, or NIL when it succeeded.
CODE is the HTTP status code and PAYLOAD the parsed body."
  (let ((err (cdr (assq 'error payload)))
        (messages (cdr (assq 'messages payload))))
    (cond ((and code (>= code 400))
           (concat (format "HTTP %d" code)
                   (cond ((stringp err) (concat ": " err))
                         (messages
                          (concat ": " (mapconcat
                                        (lambda (m)
                                          (format "%s" (or (and (consp m)
                                                                (cdr (assq 'text m)))
                                                           m)))
                                        messages "; ")))
                         (err (format ": %S" err))
                         (t ""))))
          ((stringp err) err))))

(defun p4-swarm--payload (response)
  "Return the useful part of RESPONSE, unwrapping the Swarm `data' envelope."
  (or (cdr (assq 'data response)) response))

(defun p4-swarm--request-headers (body)
  "Return the HTTP headers for a Swarm request carrying BODY."
  (append `(("Authorization" . ,(p4-swarm--auth-header))
            ("Accept" . "application/json"))
          (when body '(("Content-Type" . "application/json")))))

(defun p4-swarm--request-sync (method path &optional params body)
  "Send a METHOD request to Swarm PATH and return the parsed JSON payload.
PARAMS is a query alist and BODY, when non-NIL, an alist sent as a JSON body.
Signals a `user-error' when the request fails."
  (let* ((url-request-method method)
         (url-request-extra-headers (p4-swarm--request-headers body))
         (url-request-data (and body (encode-coding-string (json-encode body) 'utf-8)))
         (target (p4-swarm--url path params))
         (buffer (url-retrieve-synchronously target t t p4-swarm-timeout))
         parsed)
    (unless buffer
      (user-error "Swarm: no response from %s" target))
    (unwind-protect
        (with-current-buffer buffer
          (setq parsed (p4-swarm--parse-response)))
      (kill-buffer buffer))
    (let ((err (p4-swarm--response-error (car parsed) (cdr parsed))))
      (when err (user-error "Swarm: %s" err)))
    (cdr parsed)))

(defun p4-swarm--request-async (method path params body callback)
  "Send a METHOD request to Swarm PATH and pass the parsed payload to CALLBACK.
PARAMS is a query alist and BODY, when non-NIL, an alist sent as a JSON body.
Failures are reported with `message' and CALLBACK is not called."
  (let* ((url-request-method method)
         (url-request-extra-headers (p4-swarm--request-headers body))
         (url-request-data (and body (encode-coding-string (json-encode body) 'utf-8))))
    (url-retrieve
     (p4-swarm--url path params)
     (lambda (status)
       (let (parsed)
         (unwind-protect
             (setq parsed (p4-swarm--parse-response))
           (kill-buffer (current-buffer)))
         (let ((err (or (p4-swarm--response-error (car parsed) (cdr parsed))
                        (and (null (car parsed))
                             (plist-get status :error)
                             (format "%S" (plist-get status :error))))))
           (if err
               (message "Swarm: %s" err)
             (funcall callback (cdr parsed))))))
     nil t t)))

(defun p4-swarm--server-version ()
  "Return the Swarm server version string, or NIL when it cannot be read."
  (let ((buffer (ignore-errors
                  (url-retrieve-synchronously
                   (concat (p4-swarm--base-url) "/api/version")
                   t t p4-swarm-timeout))))
    (when buffer
      (unwind-protect
          (with-current-buffer buffer
            (let ((payload (p4-swarm--payload (cdr (p4-swarm--parse-response)))))
              (cdr (assq 'version payload))))
        (kill-buffer buffer)))))

(defun p4-swarm-check-auth ()
  "Check that the Swarm API is reachable and the configured credentials work.

Requires these environment variables:
  P4SWARMURL    Root URL of the Swarm server.
  P4USER        Your Perforce user name.
  P4SWARMTOKEN  A Swarm API token, from your Swarm profile page."
  (interactive)
  (p4-swarm--check-config)
  (let* ((version (p4-swarm--server-version))
         (payload (p4-swarm--payload
                   (p4-swarm--request-sync "GET" "/reviews" '(("max" . 1)))))
         (reviews (cdr (assq 'reviews payload))))
    (message "Swarm %s at %s: authenticated as %s, %d review%s readable"
             (or version "(version unknown)")
             (p4-swarm--base-url)
             (p4-swarm--user)
             (length reviews)
             (if (= 1 (length reviews)) "" "s"))))

;;; Review list:

(defun p4-swarm--true-p (value)
  "Return non-NIL unless VALUE is NIL or JSON false."
  (and value (not (eq value :json-false))))

(defun p4-swarm--matches-role-p (review user role)
  "Return non-NIL when USER holds ROLE on REVIEW.
ROLE is one of `p4-swarm-roles'."
  (let ((author (cdr (assq 'author review)))
        (participants (cdr (assq 'participants review))))
    (cond ((equal role "author") (equal author user))
          ((equal role "reviewer") (and (member user participants)
                                        (not (equal author user))))
          (t (or (equal author user) (and (member user participants) t))))))

(defun p4-swarm--review-url (id)
  "Return the Swarm web URL for the review with id ID."
  (format "%s/reviews/%s" (p4-swarm--base-url) id))

(defun p4-swarm--format-date (seconds)
  "Format SECONDS, a Unix timestamp, as a date; empty string when not a number."
  (if (numberp seconds) (format-time-string "%Y-%m-%d" seconds) ""))

(defun p4-swarm--format-time (seconds)
  "Format SECONDS, a Unix timestamp, as a date and time."
  (if (numberp seconds) (format-time-string "%Y-%m-%d %H:%M" seconds) ""))

(defun p4-swarm--vote-summary (review)
  "Return the up and down vote tally of REVIEW as a string."
  (let ((up 0) (down 0))
    (dolist (entry (cdr (assq 'participantsData review)))
      (let ((value (cdr (assq 'value (cdr (assq 'vote (cdr entry)))))))
        (cond ((and (numberp value) (> value 0)) (setq up (1+ up)))
              ((and (numberp value) (< value 0)) (setq down (1+ down))))))
    (format "+%d/-%d" up down)))

(defun p4-swarm--review-summary (review)
  "Return the first line of the description of REVIEW."
  (let ((description (or (cdr (assq 'description review)) "")))
    (string-trim (or (car (split-string description "\n" t)) ""))))

(defun p4-swarm--review-entry (review)
  "Return REVIEW as a `tabulated-list-mode' entry."
  (let ((id (cdr (assq 'id review))))
    (list id
          (vector (propertize (format "%s" id)
                              'mouse-face 'highlight
                              'help-echo (concat "mouse-1, RET: open "
                                                 (p4-swarm--review-url id))
                              'follow-link t)
                  (or (cdr (assq 'stateLabel review))
                      (cdr (assq 'state review))
                      "")
                  (or (cdr (assq 'author review)) "")
                  (p4-swarm--format-date (cdr (assq 'updated review)))
                  (p4-swarm--vote-summary review)
                  (p4-swarm--review-summary review)))))

(defvar p4-swarm-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'p4-swarm-browse-review)
    (define-key map "o" 'p4-swarm-browse-review)
    (define-key map "w" 'p4-swarm-browse-review)
    (define-key map "c" 'p4-swarm-list-comments)
    (define-key map "d" 'p4-swarm-review-diff)
    (define-key map "C" 'p4-swarm-comment-on-review)
    (define-key map "v" 'p4-swarm-vote)
    (define-key map "+" 'p4-swarm-vote-up)
    (define-key map "-" 'p4-swarm-vote-down)
    (define-key map "=" 'p4-swarm-vote-clear)
    (define-key map "g" 'p4-swarm-refresh)
    (define-key map "q" 'quit-window)
    (define-key map [mouse-1] 'p4-swarm-mouse-browse-review)
    (define-key map [follow-link] 'mouse-face)
    map)
  "The key map to use in P4 Swarm List Mode.")

(define-derived-mode p4-swarm-list-mode tabulated-list-mode "P4 Swarm"
  "Major mode for browsing a list of Swarm code reviews.
\\{p4-swarm-list-mode-map}"
  (setq tabulated-list-format
        [("ID"           9 t)
         ("State"       18 t)
         ("Author"      20 t)
         ("Updated"     11 t)
         ("Votes"        8 t)
         ("Description"  0 t)])
  (setq tabulated-list-sort-key '("Updated" . t))
  (tabulated-list-init-header))

(defvar-local p4-swarm--query nil
  "Plist of the query that produced this Swarm review list.")

(defvar-local p4-swarm--reviews nil
  "Hash table mapping review id to its record, for this review list.")

(defvar-local p4-swarm--review-id nil
  "Id of the Swarm review this buffer relates to.")

(defvar-local p4-swarm--review-version nil
  "Version number of the Swarm review revision this buffer relates to.")

(defun p4-swarm--buffer-name (query)
  "Return the review list buffer name for QUERY."
  (format "*P4 Swarm: %s as %s, %s*"
          (plist-get query :user)
          (plist-get query :role)
          (plist-get query :state)))

(defun p4-swarm--display-reviews (query reviews)
  "Render REVIEWS, the result of QUERY, in a `p4-swarm-list-mode' buffer."
  (if (null reviews)
      (message "Swarm: no %s reviews with %s as %s"
               (plist-get query :state)
               (plist-get query :user)
               (plist-get query :role))
    (let ((buffer (get-buffer-create (p4-swarm--buffer-name query))))
      (with-current-buffer buffer
        (p4-swarm-list-mode)
        (setq default-directory (or (plist-get query :dir) default-directory))
        (setq p4-swarm--query query)
        (setq p4-swarm--reviews (make-hash-table :test 'equal))
        (dolist (review reviews)
          (puthash (cdr (assq 'id review)) review p4-swarm--reviews))
        (setq tabulated-list-entries (mapcar #'p4-swarm--review-entry reviews))
        (tabulated-list-print t)
        (goto-char (point-min)))
      (pop-to-buffer buffer)
      (message "Swarm: %d review%s with %s as %s"
               (length reviews)
               (if (= 1 (length reviews)) "" "s")
               (plist-get query :user)
               (plist-get query :role)))))

(defun p4-swarm--fetch-reviews (query after accumulated page)
  "Fetch one page of reviews for QUERY and display them when the last arrives.
QUERY is a plist of :user, :role, :state and :max.  AFTER is the `lastSeen'
cursor from the previous page, or NIL for the first.  ACCUMULATED holds the
reviews matching the role so far and PAGE counts the requests made."
  (let* ((user (plist-get query :user))
         (role (plist-get query :role))
         (max (plist-get query :max))
         (states (cdr (assoc (plist-get query :state) p4-swarm-state-filters)))
         (params (append `(("max" . ,p4-swarm-page-size)
                           ("keywords" . ,user)
                           ("keywordsFields[]" . ,p4-swarm-keywords-fields)
                           ("fields[]" . ,p4-swarm-review-fields))
                         (when states `(("state[]" . ,states)))
                         (when after `(("after" . ,after))))))
    (p4-swarm--request-async
     "GET" "/reviews" params nil
     (lambda (response)
       (let* ((payload (p4-swarm--payload response))
              (reviews (cdr (assq 'reviews payload)))
              (last-seen (cdr (assq 'lastSeen payload)))
              (matched (append accumulated
                               (cl-remove-if-not
                                (lambda (review)
                                  (p4-swarm--matches-role-p review user role))
                                reviews))))
         (if (and reviews last-seen
                  (< (length matched) max)
                  (< page p4-swarm-max-pages))
             (progn
               (message "Swarm: %d matching review%s after %d page%s..."
                        (length matched) (if (= 1 (length matched)) "" "s")
                        page (if (= 1 page) "" "s"))
               (p4-swarm--fetch-reviews query last-seen matched (1+ page)))
           (p4-swarm--display-reviews
            query (if (> (length matched) max)
                      (cl-subseq matched 0 max)
                    matched))))))))

(defun p4-swarm-list-reviews (user role state &optional max)
  "List Swarm code reviews on which USER holds ROLE, restricted to STATE.

ROLE is one of `p4-swarm-roles' and STATE one of `p4-swarm-states'.  MAX caps
the number of reviews listed and defaults to `p4-swarm-default-max-reviews';
interactively it comes from the prefix argument.

Requires these environment variables:
  P4SWARMURL    Root URL of the Swarm server.
  P4USER        Your Perforce user name.
  P4SWARMTOKEN  A Swarm API token, from your Swarm profile page.

Results are shown in `p4-swarm-list-mode', from which the review can be opened
in a browser, its diff and comments read, commented on, and voted on."
  (interactive
   (list (p4-completing-read 'user "Reviews for user: " (getenv "P4USER"))
         (completing-read "Role: " p4-swarm-roles nil t nil nil "participant")
         (completing-read "State: " p4-swarm-states nil t nil nil "open")
         (and current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (p4-swarm--check-config)
  (let ((query (list :user user :role role :state state
                     :max (or max p4-swarm-default-max-reviews)
                     :dir default-directory)))
    (message "Swarm: fetching %s reviews for %s..." state user)
    (p4-swarm--fetch-reviews query nil nil 1)))

;;; Review context shared by the list, diff and comment buffers:

(defun p4-swarm--current-review-id ()
  "Return the id of the Swarm review the current context refers to."
  (or (and (derived-mode-p 'p4-swarm-list-mode) (tabulated-list-get-id))
      p4-swarm--review-id
      (user-error "No Swarm review here")))

(defun p4-swarm--review-details (id)
  "Return the full Swarm record for the review with id ID, including versions."
  (let ((payload (p4-swarm--payload
                  (p4-swarm--request-sync "GET" (format "/reviews/%s" id)))))
    (or (cdr (assq 'review payload))
        (car (cdr (assq 'reviews payload)))
        payload)))

(defun p4-swarm--latest-version (review)
  "Return (NUMBER . VERSION) for the newest revision of REVIEW.
NUMBER is the 1-based version number Swarm uses in comment and vote requests."
  (let* ((versions (cdr (assq 'versions review)))
         (count (length versions)))
    (when (zerop count)
      (user-error "Swarm review %s has no revisions" (cdr (assq 'id review))))
    (cons count (nth (1- count) versions))))

(defun p4-swarm--current-review-version ()
  "Return the version number of the Swarm review revision in play here.
Falls back to fetching the review and using its newest revision."
  (or p4-swarm--review-version
      (car (p4-swarm--latest-version
            (p4-swarm--review-details (p4-swarm--current-review-id))))))

(defun p4-swarm-browse-review ()
  "Open the Swarm review at point in a web browser."
  (interactive)
  (browse-url (p4-swarm--review-url (p4-swarm--current-review-id))))

(defun p4-swarm-mouse-browse-review (event)
  "Open the Swarm review clicked on by EVENT in a web browser."
  (interactive "e")
  (mouse-set-point event)
  (p4-swarm-browse-review))

(defun p4-swarm-refresh ()
  "Re-run the query that produced this Swarm review list."
  (interactive)
  (let ((query p4-swarm--query))
    (unless query (user-error "Not a Swarm review list buffer"))
    (message "Swarm: refreshing...")
    (p4-swarm--fetch-reviews query nil nil 1)))

(defun p4-swarm--refresh-review-row (id)
  "Re-fetch the review with id ID and update its row in the current list."
  (when (and (derived-mode-p 'p4-swarm-list-mode) p4-swarm--reviews)
    (let ((review (p4-swarm--review-details id)))
      (puthash id review p4-swarm--reviews)
      (setq tabulated-list-entries
            (mapcar (lambda (entry)
                      (if (equal (car entry) id)
                          (p4-swarm--review-entry review)
                        entry))
                    tabulated-list-entries))
      (tabulated-list-print t))))

;;; Review diff:

(defvar p4-swarm-diff-file-regexp "^==== \\(//[^#\n]+\\)#[0-9]+ "
  "Regexp matching a file header in `p4 describe' output.
Group 1 is the depot path of the file, without its revision.")

(defvar p4-swarm-diff-hunk-regexp
  "^@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@"
  "Regexp matching a unified diff hunk header.
Groups 1 and 2 are the first line numbers of the left and right side.")

(defvar p4-swarm-diff-mode-map
  (let ((map (p4-make-derived-map p4-diff-mode-map)))
    (define-key map "C" 'p4-swarm-comment-on-line)
    (define-key map "c" 'p4-swarm-list-comments)
    (define-key map "v" 'p4-swarm-vote)
    (define-key map "w" 'p4-swarm-browse-review)
    (define-key map "g" 'p4-swarm-review-diff)
    map)
  "The key map to use in P4 Swarm Diff Mode.")

(define-derived-mode p4-swarm-diff-mode p4-diff-mode "P4 Swarm Diff"
  "Major mode for the diff of a Swarm code review.
\\{p4-swarm-diff-mode-map}")

(defun p4-swarm--version-diff-args (version)
  "Return the `p4 describe' arguments that diff the review revision VERSION."
  (let* ((archive (cdr (assq 'archiveChange version)))
         (change (cdr (assq 'change version)))
         (pending (p4-swarm--true-p (cdr (assq 'pending version))))
         (target (or archive change)))
    (unless target
      (user-error "Swarm review revision has no changelist to diff"))
    (append (list "describe")
            (p4-make-list-from-string p4-default-diff-options)
            (when (or archive pending) (list "-S"))
            (list (format "%s" target)))))

(defun p4-swarm-review-diff ()
  "Show the diff of the newest revision of the Swarm review at point.

The diff is produced locally by `p4 describe' against the changelist Swarm
holds the revision in, so all of `p4-diff-mode' navigation works in it.  Press
\\<p4-swarm-diff-mode-map>\\[p4-swarm-comment-on-line] on a line to comment on
that line of that file."
  (interactive)
  (let* ((id (p4-swarm--current-review-id))
         (review (progn (message "Swarm: fetching review %s..." id)
                        (p4-swarm--review-details id)))
         (latest (p4-swarm--latest-version review))
         (version (car latest))
         (args (p4-swarm--version-diff-args (cdr latest)))
         (buffer (p4-make-output-buffer
                  (format "*P4 Swarm Diff: review %s v%s*" id version)
                  'p4-swarm-diff-mode)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (p4-run args)
        (p4-activate-diff-buffer))
      (setq p4-swarm--review-id id)
      (setq p4-swarm--review-version version)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun p4-swarm--diff-line-numbers (body-start target left right)
  "Return (LEFT-LINE . RIGHT-LINE) for the diff line starting at TARGET.
BODY-START is the position of the first line after the hunk header, and LEFT and
RIGHT that hunk's starting line numbers.  Either element of the result is NIL
when the target line is not present on that side of the diff."
  (save-excursion
    (goto-char body-start)
    (let ((result nil))
      (while (and (null result) (<= (point) target) (not (eobp)))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (kind (if (zerop (length line)) ?\s (aref line 0))))
          (when (memq kind '(?@ ?=))
            (user-error "Point is not inside a diff hunk"))
          (if (= (line-beginning-position) target)
              (setq result (cons (and (memq kind '(?\s ?-)) left)
                                 (and (memq kind '(?\s ?+)) right)))
            (cond ((eq kind ?\s) (setq left (1+ left) right (1+ right)))
                  ((eq kind ?+) (setq right (1+ right)))
                  ((eq kind ?-) (setq left (1+ left)))))
          (forward-line 1)))
      (or result (user-error "Point is not inside a diff hunk")))))

(defun p4-swarm--diff-content-at-point (body-start)
  "Return the diff line at point and up to four before it as a vector.
BODY-START limits how far back lines are collected."
  (save-excursion
    (beginning-of-line)
    (let ((lines (list (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))
          (remaining p4-swarm-comment-context-lines))
      (while (and (> remaining 0) (> (line-beginning-position) body-start))
        (forward-line -1)
        (push (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))
              lines)
        (setq remaining (1- remaining)))
      (vconcat lines))))

(defun p4-swarm--diff-context-at-point ()
  "Return the Swarm comment context for the diff line at point.
The result is an alist with `file', `leftLine', `rightLine' and `content' keys,
ready to be sent as the context of an inline comment.  Signals a `user-error'
when point is not on a line inside a hunk of a recognisable file."
  (save-excursion
    (beginning-of-line)
    (let* ((target (point))
           (file-pos (save-excursion
                       (and (re-search-backward p4-swarm-diff-file-regexp nil t)
                            (point))))
           (file (and file-pos (match-string-no-properties 1)))
           (hunk (save-excursion
                   (and (re-search-backward p4-swarm-diff-hunk-regexp nil t)
                        (list (point)
                              (string-to-number (match-string 1))
                              (string-to-number (match-string 2))
                              (line-beginning-position 2))))))
      (unless file
        (user-error "No \"==== file ====\" header above point"))
      (unless (and hunk (> (nth 0 hunk) file-pos))
        (user-error "Point is not inside a diff hunk of %s" file))
      (let* ((body-start (nth 3 hunk))
             (numbers (p4-swarm--diff-line-numbers
                       body-start target (nth 1 hunk) (nth 2 hunk))))
        (list (cons 'file file)
              (cons 'leftLine (car numbers))
              (cons 'rightLine (cdr numbers))
              (cons 'content (p4-swarm--diff-content-at-point body-start)))))))

;;; Comments:

(defun p4-swarm--comment-location (context)
  "Return \"FILE:LINE\" for the comment CONTEXT, or NIL when it has no file."
  (let ((file (cdr (assq 'file context)))
        (line (or (cdr (assq 'rightLine context)) (cdr (assq 'leftLine context)))))
    (when file
      (if line (format "%s:%s" file line) file))))

(defun p4-swarm--comment-target (id context)
  "Describe where a comment on review ID with CONTEXT will be posted."
  (let ((location (p4-swarm--comment-location context)))
    (if location
        (format "%s in review %s" location id)
      (format "review %s" id))))

(defvar p4-swarm-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'p4-swarm-comment-send)
    (define-key map (kbd "C-c C-k") 'p4-swarm-comment-abort)
    map)
  "The key map to use in P4 Swarm Comment Mode.")

(define-derived-mode p4-swarm-comment-mode text-mode "P4 Swarm Comment"
  "Major mode for composing a comment on a Swarm code review.
\\<p4-swarm-comment-mode-map>Send the comment with \\[p4-swarm-comment-send] or
abandon it with \\[p4-swarm-comment-abort].")

(defvar-local p4-swarm--comment-context nil
  "Diff context a comment composed in this buffer will be anchored to.")

(defvar-local p4-swarm--comment-window-config nil
  "Window configuration to restore when this comment buffer goes away.")

(defun p4-swarm--compose-comment (id version context)
  "Pop up a buffer for composing a comment on the review with id ID.
CONTEXT, when non-NIL, anchors the comment to a file and line of revision
VERSION of the review."
  (let ((config (current-window-configuration))
        (buffer (get-buffer-create (format "*P4 Swarm Comment: review %s*" id))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (p4-swarm-comment-mode)
      (setq p4-swarm--review-id id)
      (setq p4-swarm--review-version version)
      (setq p4-swarm--comment-context context)
      (setq p4-swarm--comment-window-config config)
      (setq header-line-format
            (format "Comment on %s -- C-c C-c to send, C-c C-k to abandon"
                    (p4-swarm--comment-target id context))))
    (pop-to-buffer buffer)))

(defun p4-swarm--close-comment-buffer ()
  "Kill the current comment buffer and restore the previous window layout."
  (let ((config p4-swarm--comment-window-config)
        (buffer (current-buffer)))
    (kill-buffer buffer)
    (when (and config (frame-live-p (window-configuration-frame config)))
      (set-window-configuration config))))

(defun p4-swarm-comment-send ()
  "Post the comment composed in this buffer to Swarm."
  (interactive)
  (let ((body (string-trim (buffer-substring-no-properties (point-min) (point-max))))
        (id p4-swarm--review-id)
        (version p4-swarm--review-version)
        (context p4-swarm--comment-context))
    (unless id (user-error "Not a Swarm comment buffer"))
    (when (string-empty-p body) (user-error "Comment is empty"))
    (when (yes-or-no-p (format "Post this comment on %s? "
                               (p4-swarm--comment-target id context)))
      (p4-swarm--request-sync
       "POST" (format "/comments/reviews/%s" id) nil
       (append (list (cons 'body body))
               (when context
                 (list (cons 'context
                             (if version
                                 (append context (list (cons 'version version)))
                               context))))))
      (message "Swarm: comment posted on %s" (p4-swarm--comment-target id context))
      (p4-swarm--close-comment-buffer))))

(defun p4-swarm-comment-abort ()
  "Abandon the comment composed in this buffer."
  (interactive)
  (when (or (string-empty-p
             (string-trim (buffer-substring-no-properties (point-min) (point-max))))
            (yes-or-no-p "Discard this comment? "))
    (p4-swarm--close-comment-buffer)))

(defun p4-swarm-comment-on-review ()
  "Comment on the Swarm review at point as a whole."
  (interactive)
  (p4-swarm--compose-comment (p4-swarm--current-review-id) nil nil))

(defun p4-swarm-comment-on-line ()
  "Comment on the file and line of the Swarm review diff at point."
  (interactive)
  (unless (derived-mode-p 'p4-swarm-diff-mode)
    (user-error "Not in a Swarm review diff buffer"))
  (p4-swarm--compose-comment p4-swarm--review-id
                             p4-swarm--review-version
                             (p4-swarm--diff-context-at-point)))

(defvar p4-swarm-comments-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") 'p4-swarm-comments-visit-context)
    (define-key map "g" 'p4-swarm-comments-refresh)
    (define-key map "w" 'p4-swarm-browse-review)
    (define-key map "o" 'p4-swarm-browse-review)
    (define-key map "d" 'p4-swarm-review-diff)
    (define-key map "C" 'p4-swarm-comment-on-review)
    (define-key map "v" 'p4-swarm-vote)
    map)
  "The key map to use in P4 Swarm Comments Mode.")

(define-derived-mode p4-swarm-comments-mode special-mode "P4 Swarm Comments"
  "Major mode for reading the comments on a Swarm code review.
\\{p4-swarm-comments-mode-map}")

(defun p4-swarm--comment-list (payload)
  "Return the comments in PAYLOAD as a list.
Swarm has returned them both as an array and as an object keyed by comment id."
  (let ((comments (cdr (assq 'comments payload))))
    (if (and (consp comments) (consp (car comments)) (symbolp (caar comments)))
        (mapcar #'cdr comments)
      comments)))

(defun p4-swarm--insert-comment (comment)
  "Insert COMMENT into the current buffer."
  (let ((start (point))
        (context (cdr (assq 'context comment)))
        (task-state (cdr (assq 'taskState comment))))
    (insert (propertize (format "%s  %s"
                                (or (cdr (assq 'user comment)) "?")
                                (p4-swarm--format-time (cdr (assq 'time comment))))
                        'face 'bold))
    (when (and task-state (not (equal task-state "comment")))
      (insert (propertize (format "  [%s]" task-state) 'face 'warning)))
    (insert "\n")
    (let ((location (p4-swarm--comment-location context)))
      (when location
        (insert (propertize (format "  %s\n" location) 'face 'shadow))))
    (dolist (line (split-string (or (cdr (assq 'body comment)) "") "\n"))
      (insert "  " line "\n"))
    (insert "\n")
    (when context
      (put-text-property start (point) 'p4-swarm-context context))))

(defun p4-swarm--display-comments (id comments dir)
  "Render COMMENTS on the review with id ID, newest first.
DIR becomes the `default-directory' of the buffer, so that visiting the file a
comment refers to runs `p4' in the right client."
  (let ((buffer (get-buffer-create (format "*P4 Swarm Comments: review %s*" id)))
        (sorted (sort (copy-sequence comments)
                      (lambda (a b) (> (or (cdr (assq 'time a)) 0)
                                       (or (cdr (assq 'time b)) 0))))))
    (with-current-buffer buffer
      (p4-swarm-comments-mode)
      (setq default-directory (or dir default-directory))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize
                 (format "Review %s -- %d comment%s, newest first\n\n"
                         id (length sorted) (if (= 1 (length sorted)) "" "s"))
                 'face 'font-lock-comment-face))
        (if (null sorted)
            (insert "No comments.\n")
          (mapc #'p4-swarm--insert-comment sorted)))
      (setq p4-swarm--review-id id)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun p4-swarm-list-comments ()
  "Show the comments on the Swarm review at point, newest first."
  (interactive)
  (let ((id (p4-swarm--current-review-id))
        (dir default-directory))
    (message "Swarm: fetching comments on review %s..." id)
    (p4-swarm--request-async
     "GET" (format "/comments/reviews/%s" id)
     `(("max" . ,p4-swarm-page-size) ("ignoreArchived" . "true"))
     nil
     (lambda (response)
       (p4-swarm--display-comments
        id (p4-swarm--comment-list (p4-swarm--payload response)) dir)))))

(defun p4-swarm-comments-refresh ()
  "Re-fetch the comments shown in this buffer."
  (interactive)
  (unless p4-swarm--review-id (user-error "Not a Swarm comments buffer"))
  (p4-swarm-list-comments))

(defun p4-swarm-comments-visit-context ()
  "Visit the file and line that the comment at point refers to."
  (interactive)
  (let* ((context (get-text-property (point) 'p4-swarm-context))
         (file (cdr (assq 'file context)))
         (line (or (cdr (assq 'rightLine context)) (cdr (assq 'leftLine context)))))
    (unless file (user-error "No file context on the comment at point"))
    (p4-depot-find-file file (and (numberp line) line))))

;;; Voting:

(defun p4-swarm-vote (vote)
  "Cast VOTE on the Swarm review at point.
VOTE is \"up\", \"down\" or \"clear\".  The vote is cast on the newest revision
of the review; Swarm rejects votes on older ones."
  (interactive
   (list (completing-read "Vote: " '("up" "down" "clear") nil t nil nil "up")))
  (let ((id (p4-swarm--current-review-id))
        (version (p4-swarm--current-review-version)))
    (when (yes-or-no-p (format "Vote %s on review %s (revision %s)? "
                               vote id version))
      (p4-swarm--request-sync "POST" (format "/reviews/%s/vote" id) nil
                              (list (cons 'vote vote) (cons 'version version)))
      (message "Swarm: voted %s on review %s" vote id)
      (p4-swarm--refresh-review-row id))))

(defun p4-swarm-vote-up ()
  "Vote up the Swarm review at point."
  (interactive)
  (p4-swarm-vote "up"))

(defun p4-swarm-vote-down ()
  "Vote down the Swarm review at point."
  (interactive)
  (p4-swarm-vote "down"))

(defun p4-swarm-vote-clear ()
  "Clear your vote on the Swarm review at point."
  (interactive)
  (p4-swarm-vote "clear"))

(provide 'p4-swarm)
