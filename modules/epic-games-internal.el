(require 'p4)
(require 'project)

(defun epic-launch-submit-tool ()
  "Launch Epic's SubmitTool."
  (interactive)
  (let* ((p4port (getenv "P4PORT"))
         (p4user (getenv "P4USER"))
         (p4client (getenv "P4CLIENT"))
         (localappdata (getenv "LOCALAPPDATA"))
         (root-dir (string-trim-right (shell-command-to-string "p4 -F %clientRoot% -ztag info")))
         (cl (p4-completing-read 'shelved "Changelist: "))
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
  (when-let ((root (locate-dominating-file path #'project-root-p)))
    (cons 'transient (expand-file-name root))))
; This really slows down project-files due to the sheer size of the project. Need to be careful here.
; (add-to-list 'project-find-functions #'project-find-root)

; Allow projectile to recognize the root of the Unreal workspace.
;; (with-eval-after-load 'projectile
;;   (projectile-register-project-type 'ue5 '("Default.uprojectdirs")
;;                                     :project-file "Default.uprojectdirs"))

(defun find-public-or-private-directory (path)
  "Find either 'Public' or 'Private' directory presence."
  (or (locate-dominating-file path "Public")
      (locate-dominating-file path "Private")))

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
           (private-dir (concat module-root-dir "Private/" module-relative-path)))
      (setq cc-search-directories (nconc cc-search-directories `(,public-dir ,private-dir))))))

(defun ue-ff-restore-search-directories ()
  (custom-reevaluate-setting 'cc-search-directories))

(add-hook 'ff-pre-find-hooks 'ue-ff-other-file-alist-function)
(add-hook 'ff-post-load-hooks 'ue-ff-restore-search-directories)
(add-hook 'ff-not-found-hooks 'ue-ff-restore-search-directories)
(add-hook 'ff-file-created-hook 'ue-ff-restore-search-directories)

(provide 'epic-games-internal)
