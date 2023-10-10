; TODO Implement unshelving between branches by allowing to choose a branch mapping to use.
; Can retrieve the list of branch mappings fairly easily so this should allow for auto-complete as well.

(require 'p4)

;; (defp4cmd p4-sync-changelist-files (&rest args)
;;   "sync"
;;   "Force syncs only the file(s) in the specified changelist."
;;   (interactive
;;    (if current-prefix-arg
;;        (p4-read-args "p4 sync -f" "" 'shelved)
;;      (append (list "-p" "-f" "-s" (p4-completing-read 'shelved "Copy from: "))
;;              (when p4-open-in-changelist
;;                (list "-c" (p4-completing-read 'pending "New/existing shelf: "))))))
;;   (p4-call-command "reshelve" args :mode 'p4-basic-list-mode
;;                    :callback (p4-refresh-callback)))

; TODO Implement this. Basically needs to get a list of files to sync from `p4 opened` and then 
; call `p4 sync -f` on each of them. Which is essentially `p4-refresh` already.
; (defp4cmd p4-sync-changelist-files (&rest args)
;   "sync"
;   "Force syncs only the file(s) in the specified changelist."
;   (interactive
;    (if current-prefix-arg
;        (p4-read-args "p4 sync" "" 'shelved)
;      (append (list "-f" "-c" (p4-completing-read 'shelved "Changelist: ")))))
;   (p4-call-command "sync" args :mode 'p4-basic-list-mode))


(defp4cmd p4-reshelve (&rest args)
  "reshelve"
  "Copies shelved files from an existing shelf into either a new shelf or one that has already been created."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 reshelve:" "" 'shelved)
     (append (list "-p" "-f" "-s" (p4-completing-read 'shelved "Copy from: "))
             (when p4-open-in-changelist
               (list "-c" (p4-completing-read 'pending "New/existing shelf: "))))))
  (p4-call-command "reshelve" args :mode 'p4-basic-list-mode
                   :callback (p4-refresh-callback)))

(defp4cmd p4-opened-files-in-changelist (&rest args)
  "opened-list"
  "Just lists the files in a given changelist, without any other information."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 opened:" "" 'pending)
     (append (list "%depotFile%" "opened" "-c" (p4-completing-read 'shelved "Changelist: ")))))
  (p4-call-command "-F" args :mode 'p4-basic-list-mode))

(defun p4-call-process-shell-command (&optional infile destination display &rest args)
    ""
    (apply #'call-process-shell-command (concat (p4-executable) " " (funcall p4-modify-args-function args)) infile destination display))

(defun p4-start-process-shell-command (name buffer &rest program-args)
  "Similar to `p4-start-process`, except that the command is passed to a shell instead of 
  executing it directly. This allows piping in commands to be used, since otherwise it's 
  not really a single command that can be run."
  (message "start cmd: %s" (concat "\"" (p4-executable) "\"" " " (mapconcat 'identity (funcall p4-modify-args-function (car program-args)) " ")))
  (start-process-shell-command name buffer (concat "\"" (p4-executable) "\"" " " (mapconcat 'identity (funcall p4-modify-args-function (car program-args)) " "))))

(defun p4-process-shell-restart()
  ""
  (interactive)
  (unless p4-process-args
    (error "Can't restart Perforce process in this buffer."))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if p4-process-synchronous
        (p4-with-coding-system
          (let ((status (apply #'p4-call-process-shell-command nil t nil
                               p4-process-args)))
            (p4-process-finished (current-buffer) "P4"
                                 (if (zerop status) "finished\n"
                                   (format "exited with status %d\n" status)))))
      (let ((process (apply #'p4-start-process-shell-command "P4" (current-buffer)
                            p4-process-args)))
        (set-process-query-on-exit-flag process nil)
        (set-process-sentinel process 'p4-process-sentinel)
        (p4-set-process-coding-system process)
        (message "Run: p4 %s" (p4-join-list (car p4-process-args)))))))

(defun* p4-call-shell-command (cmd &optional args &key mode callback after-show
                             (auto-login t) synchronous pop-up-output)
  ""
  (with-current-buffer
      (p4-make-output-buffer (format "*P4 %s*" (mapconcat 'identity cmd)) mode)
    (set (make-local-variable 'revert-buffer-function) 'p4-revert-buffer)
    (setq p4-process-args (cons cmd args)
          p4-process-after-show after-show
          p4-process-auto-login auto-login
          p4-process-callback callback
          p4-process-pop-up-output pop-up-output
          p4-process-synchronous nil)
    (p4-process-shell-restart)))

; Command is `p4 -F %depotFile% opened -c 1234 | p4 -x - reopen -c 5678`
; to move files from 1234 to 5678
(defp4cmd p4-move-files-from-changelist (&rest args)
  "move-files-from-changelist"
  "Moves files between changelists."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 move-files-from-changelist:" "" 'pending)
     (list "-F" "%depotFile%" "opened" "-c" (p4-completing-read 'pending "Move files from: ") "|"
                   "p4" "-x" "-" "reopen" "-c" (p4-completing-read 'pending "Move files to: "))))
    (p4-call-shell-command args))

(defp4cmd p4-shelve-force (&rest args)
  "shelve"
  "Store files (or a stream spec) from a pending changelist in the depot, without submitting them."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 shelve" "" 'pending)
     (append (list "-p" "-r" "-c" (p4-completing-read 'pending "Changelist: ")))))
  (p4-call-command "shelve" args :mode 'p4-basic-list-mode))

(defp4cmd p4-shelve-discard-files (&rest args)
  "shelve"
  "Discards shelved files (or a stream spec) for a pending changelist in the depot."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 shelve" "" 'shelved)
     (append (list "-d" "-c" (p4-completing-read 'shelved "Changelist: ")))))
  (p4-call-command "shelve" args :mode 'p4-basic-list-mode))

(defp4cmd p4-change-delete (&rest args)
  "change"
  "Delete the changelist. This is only allowed if the pending changelist has no files or pending fixes."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 change" "" 'pending)
     (append (list "-d" (p4-completing-read 'pending "Changelist: ")))))
  (p4-call-command "change" args :mode 'p4-basic-list-mode))

(defp4cmd p4-revert-changelist (&rest args)
  "revert"
  "Reverts only those files in the specified changelist."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 revert" "" 'pending)
     (append (list "-c" (p4-completing-read 'pending "Changelist: ")) '("//...") )))
  (p4-call-command "revert" args :mode 'p4-basic-list-mode
                   :callback (p4-refresh-callback)))

(defp4cmd p4-revert-changelist-and-wipe (&rest args)
  "revert"
  "Reverts only those files in the specified changelist. Also deletes files marked for add."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 revert" "" 'pending)
     (append (list "-w" "-c" (p4-completing-read 'pending "Changelist: ")) '("//...") )))
  (p4-call-command "revert" args :mode 'p4-basic-list-mode
                   :callback (p4-refresh-callback)))

(defp4cmd p4-revert-changelist-if-unchanged-and-wipe (&rest args)
  "revert"
  "Reverts only those files in the specified changelist if they haven't changed. Also deletes files marked for add."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 revert" "" 'pending)
     (append (list  "-a" "-w" "-c" (p4-completing-read 'pending "Changelist: ")) '("//...") )))
  (p4-call-command "revert" args :mode 'p4-basic-list-mode
                   :callback (p4-refresh-callback)))

(defp4cmd p4-revert-changelist-if-unchanged (&rest args)
  "revert"
  "Reverts only those files in the specified changelist if they haven't changed. This leaves all other added files unchanged."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 revert" "" 'pending)
     (append (list  "-a" "-c" (p4-completing-read 'pending "Changelist: ")) '("//...") )))
  (p4-call-command "revert" args :mode 'p4-basic-list-mode
                   :callback (p4-refresh-callback)))

(defp4cmd* show-shelved-changes-for-current-workspace
  "Shows your shelved changes (up to 200) for the current client workspace."
  (list "-t" "-m" "200" "--me" "-L" "-s" "shelved" "-c" (p4-current-client))
  (p4-file-change-log "changes" args))

(defp4cmd* show-submitted-changes-for-current-workspace
  "Shows your submitted changes (up to 200) for the current client workspace."
  (list "-t" "-m" "200" "--me" "-L" "-s" "submitted" "-c" (p4-current-client))
  (p4-file-change-log "changes" args))

(defp4cmd p4-show-opened-for-changelist (&rest args)
  "opened"
  "List open files and display file status for a specific changelist."
  (interactive
    (if current-prefix-arg
       (p4-read-args "p4 opened" "" 'pending)
       (append (list "-c" (p4-completing-read 'pending "Changelist: ")))))
   (p4-call-command "opened" args :mode 'p4-opened-list-mode
     :callback (lambda ()
                 (p4-regexp-create-links "\\<change \\([1-9][0-9]*\\) ([a-z]+)"
                                       'pending "Edit change"))
     :pop-up-output (lambda () t)))

(defp4cmd p4-unshelve-using-branch-spec (&rest args)
  "unshelve"
  "Restore shelved files from a pending change into a workspace using a specified branch spec/mapping."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 unshelve: " "" 'shelved)
     (append (list "-f" "-s" (p4-completing-read 'shelved "Unshelve from: "))
             (when p4-open-in-changelist
               (list "-c" (p4-completing-read 'pending "Open in change: ") "-b" (p4-completing-read 'branch "Unshelve using branch spec: ") )))))
  (p4-call-command "unshelve" args :mode 'p4-basic-list-mode))

(defun p4-switch-to-client (client-name)
  "Switch the P4CLIENT environment variable to a different client workspace."
  (interactive
   (list (if current-prefix-arg
             (p4-read-args "Set P4CLIENT: " "" 'client)
           (p4-completing-read 'client "Client workspace: "))))
  (setenv "P4CLIENT" client-name)
  (message "Switched to client workspace: %s" client-name))


;; (defp4cmd p4-list-changes-between-changelists (&rest args)
;;   "changes-between"
;;   "Lists out the changes between two changelist numbers. Useful for bisecting or figuring out what changes might have triggered an issue."
;;   (interactive
;;    (if current-prefix-arg
;;        (p4-read-args "p4 changes: " "" 'submitted)
;;      (append (list "changes" (p4-completing-read 'branch "Branch: ") "..." ))))
;;   (p4-call-command "-F" args :mode 'p4-basic-list-mode))


(defalias 'p4-sync-file 'p4-refresh)

(provide 'p4-extensions)
