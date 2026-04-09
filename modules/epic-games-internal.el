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
         (buffer-name "*Epic Safe Backout Tool*"))
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

                                        ; TODO This should query recent JIRAs that "involve" me in some way and allow for completion on it.
                                        ; Needs to execute the JQL
                                        ;(
                                        ;  assignee = currentUser()
                                        ;  OR reporter = currentUser()
                                        ;  OR watcher = currentUser()
                                        ;  OR creator = currentUser()
                                        ;  OR commenter = currentUser()
                                        ;  OR description ~ "your.name"
                                        ;  OR comment ~ "your.name"
                                        ;)
                                        ;ORDER BY updated DESC
                                        ; Get that result, and make it auto-complete using the results.

(defun ue-get-solution-path (interactive)
  "Find the first .sln file in the project root."
  (let ((root (project-root (project-current))))
    (directory-files root t "\\.sln$" t)))

(defun goto-jira-issue ()
  "Opens the JIRA URL for the given issue key."
  (interactive)
  (let* ((issue (p4-completing-read 'shelved "Issue: "))
         (jira-url (getenv "JIRAURL")))
    (browse-url (concat jira-url
                        "/browse/"
                        (url-hexify-string issue)))))

(defun file-jira-issue ()
  "Opens a new JIRA issue."
  (interactive)
  (browse-url (concat jira-url
                      "secure/CreateIssue!default.jspa")))

                                        ; curl ^"https://codescout.internal.epicgames.net/api/find-in-files?branch=^%^2F^%^2FFortnite^%^2FMain^&searchStr=TestSearch^&pathsStr=^&options=^%^7B^%^22matchCase^%^22^%^3Afalse^%^2C^%^22wholeWord^%^22^%^3Afalse^%^7D^" -b ^"connect.sid=s^%^3A_3TDfkUuiSckG2L_Rb0niurkXoV8HCCF.llEw6jOwbZN6OJhmrsuUJ4dTy^%^2BCKu19YsZv2colTIhA^"

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

(defun epic-codescout--format-results (results-buffer data)
  "Format the parsed JSON DATA (an alist) into the RESULTS-BUFFER using grep-mode."
  (with-current-buffer results-buffer
    (let ((inhibit-read-only t)
          (num-matches (cdr (assoc 'numMatches data)))
          (is-truncated (let ((val (cdr (assoc 'resultsTruncated data))))
                          (if (eq val :json-false) nil val)))
          (match-count 1))
      (erase-buffer)
      (grep-mode)
      (insert (format "%s matches found.%s\n\n"
                      num-matches
                      (if is-truncated " (Results Truncated)" "")))
      (dolist (file-match (coerce (cdr (assoc 'fileMatches data)) 'list))
        (let* ((file-path (cdr (assoc 'file file-match)))
               (line-matches (coerce (cdr (assoc 'lineMatches file-match)) 'list)))
          (dolist (line-match line-matches)
            (let* ((line-num (cdr (assoc 'lineNumber line-match)))
                   (line-str (cdr (assoc 'lineStr line-match)))
                   (match-start (cdr (assoc 'matchStart line-match)))
                   (match-len (cdr (assoc 'matchLen line-match)))
                   (highlighted-str (epic-codescout--highlight-match line-str match-start match-len)))
              (insert (format "%d. %s:%s:%s\n" match-count file-path line-num highlighted-str))
              (incf match-count))))))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (read-only-mode)))

(defun epic-codescout-search-callback (status)
  "Callback that handles buggy status and dirty buffer from url.el."
  (if (or (eq status 'successful) (listp status))
      (let ((results-buffer (get-buffer-create "*codescout-search-results*")))
        (with-current-buffer (current-buffer)
          (goto-char (point-min))
          (when (re-search-forward "\n\r?\n\r?" nil t)
            (delete-region (point-min) (point)))
          (condition-case err
              (let ((json-key-type 'symbol)) ; Ensure parser creates symbols for keys
                (let ((json-data (json-read-from-string (buffer-string))))
                  (switch-to-buffer-other-window results-buffer)
                  (epic-codescout--format-results results-buffer json-data)))
            (error (message "CodeScout Result: Could not parse JSON. %s" err))))
        (kill-buffer (current-buffer)))
    (message "CodeScout search failed. Status: %s" status)))

(defun epic-codescout-search (search-term)
  "Perform a search on Epic's CodeScout for SEARCH-TERM."
  (interactive "sSearch CodeScout for: ")
  (unless (and epic-codescout-cookie (not (string-empty-p epic-codescout-cookie)))
    (error "CodeScout cookie is not set. Use `M-x customize-variable epic-codescout-cookie`"))
  (let* ((base-url "https://codescout.internal.epicgames.net/api/find-in-files")
         (params `(("branch"    "//Fortnite/Main")
                   ("searchStr" ,search-term)
                   ("pathsStr"  "")
                   ("options"   "{\"matchCase\":false,\"wholeWord\":false}")))
         (full-url (concat base-url "?" (url-build-query-string params)))
         (url-request-extra-headers `(("Cookie" . ,epic-codescout-cookie))))
    (url-retrieve full-url 'epic-codescout-search-callback)))

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
