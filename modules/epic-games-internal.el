(require 'p4)

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

(provide 'epic-games-internal)
