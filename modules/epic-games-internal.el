;; -*- lexical-binding: t; -*-
(require 'p4)
(require 'project)

; TODO This is all Windows-development-specific. Might need to eventually make this work on other platforms if needed.

(defun p4-backout-changelist ()
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
;(add-to-list 'project-find-functions #'project-find-root)

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

(add-hook 'ff-pre-find-hooks 'ue-ff-other-file-alist-function)
(add-hook 'ff-post-load-hooks 'ue-ff-restore-search-directories)
(add-hook 'ff-not-found-hooks 'ue-ff-restore-search-directories)
(add-hook 'ff-file-created-hook 'ue-ff-restore-search-directories)

(provide 'epic-games-internal)
