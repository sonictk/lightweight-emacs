(require 'p4)
(require 'projectile)

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
                                    (insert "\n[Process completed]\n")
                                    (setq buffer-read-only t))))))))
  (with-current-buffer "*Epic SubmitTool*"
    (local-set-key "q" (lambda () (interactive) (quit-window t)))))

; TODO: `projectile-register-project-type` for epic workspaces so that projectile-project-info can be determined correctly.

;; (defun epic-preflight-shelved-changelist ()
;;   "Preflight a shelved changelist."
;;   (interactive)
;; )

; Allow projectile to recognize the root of the Unreal workspace.
(with-eval-after-load 'projectile
  (projectile-register-project-type 'unrealengine '("GenerateProjectFiles.bat" "GenerateProjectFiles.sh" "GenerateProjectFiles.command")
                                    :project-file '("GenerateProjectFiles.bat" "GenerateProjectFiles.sh" "GenerateProjectFiles.command")))

                                        ; todo should also customize ff-find-other-file by doing ff-other-file-alist
(provide 'epic-games-internal)
