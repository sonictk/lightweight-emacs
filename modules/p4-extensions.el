;; -*- lexical-binding: t; -*-
(require 'p4)
; Swarm review functionality
(require 'url)
(require 'json)
; (require 'request)

; Command is `p4 -F %depotFile% opened  -c 28337241 | p4 -x - sync -f`
(defp4cmd p4-force-sync-files-in-changelist (&rest args)
  "force-sync-files-in-changelist"
  "Forces sync of the file(s) in a given changelist."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 force-sync-files-in-changelist:" "" 'pending)
     (list "-Ztag" "-F" "%depotFile%" "files" (concat "@=" (p4-completing-read 'pending "Changelist: "))  "|"
                   "p4" "-x" "-" "sync" "-f")))
    (p4-call-shell-command args))

; e.g. p4 -Ztag -F %depotFile% files @=40196354 | p4 -x - sync 
(defp4cmd p4-sync-files-in-changelist (&rest args)
          "sync-files-in-changelist"
          "Syncs the file(s) in a given changelist."
          (interactive)
          (p4-call-shell-command (list "-Ztag" "-F" "%depotFile%" "files" (concat "@=" (p4-completing-read 'pending "Changelist: "))  "|"
                                       "p4" "-x" "-" "sync")))

; TODO Not sure if this is correct.
; Command is `p4 -F %depotFile%@39506137 opened -c 28337241 | p4 -x - sync`
(defp4cmd p4-sync-files-in-changelist-to-revision (&rest args)
          "sync-files-in-changelist-to-revision"
          "Syncs the file(s) in a given changelist to a specific revision."
          (interactive)
          (p4-call-shell-command (list "-Ztag" "-F" "%depotFile%" (concat "@" (p4-completing-read 'submitted "Sync to changelist: ")) "opened" (concat "-c " (p4-completing-read 'pending "Changelist: "))  "|"
                                       "p4" "-x" "-" "sync")))

; Command is `p4 sync @=28337241`
(defp4cmd p4-sync-changelist-only (&rest args)
  "sync-changelist-only"
  "Syncs only the given changelist and no other intervening changes."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 sync-changelist-only:" "" 'submitted)
     (list "sync" (concat "@=" (p4-completing-read 'submitted "Changelist: ")) )))
    (p4-call-shell-command args))

; Command is `p4 -F %depotFile% opened -c 28337241 | p4 -x - sync -r`
(defp4cmd p4-reopen-files-in-changelist (&rest args)
  "reopen-files-in-changelist"
  "Reopens the files that are mapped to new locations in the depot in a given changelist."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 reopen-files-in-changelist:" "" 'shelved)
     (list "-F" "%depotFile%" "opened" (concat "-c " (p4-completing-read 'shelved "Changelist: "))  "|"
                   "p4" "-x" "-" "sync" "-r" )))
    (p4-call-shell-command args))

(defp4cmd p4-move-file-to-changelist (&rest args)
  "move-file-to-changelist"
  "Moves/reopens the current file in the buffer to a new changelist."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4-move-file-to-changelist:" "" 'pending)
     (list "reopen" "-c" (p4-completing-read 'pending "Changelist: ") (mapconcat 'identity (p4-context-filenames-list) " "))))
    (p4-call-shell-command args))

; Command is `p4 -F %depotFile% files @=28337241 | p4 -x - flush`
(defp4cmd p4-flush-files-in-changelist (&rest args)
  "flush-files-in-changelist"
  "Updates the server metadata for files in a given changelist without actually syncing the files."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 flush-files-in-changelist:" "" 'shelved)
     (list "-F" "%depotFile%" "files" (concat "@=" (p4-completing-read 'shelved "Changelist: "))  "|"
                   "p4" "-x" "-" "flush")))
    (p4-call-shell-command args))

; Command is `p4 -F %depotFile% files @=28337241 | p4 -x - edit`
(defp4cmd p4-edit-files-in-changelist (&rest args)
  "edit-files-in-changelist"
  "Opens the files in a given changelist for editing."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 edit-files-in-changelist:" "" 'shelved)
     (list "-F" "%depotFile%" "files" (concat "@=" (p4-completing-read 'shelved "Changelist: "))  "|"
                   "p4" "-x" "-" "edit" "-c" (p4-completing-read 'pending "Open in Changelist: "))))
    (p4-call-shell-command args))

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
        (setq p4-process-args (car p4-process-args)) ; Unwrap the extra list so that things print properly
        (p4-set-process-coding-system process)
        (message "Command executed: p4 %s" (p4-join-list p4-process-args))))))

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

; TODO This should check if you're about to blow away a shelf with empty local changes first.
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

; TODO This should just call p4-shelve-discard-files if there are already files in the changelist and provide confirmation asking.
(defp4cmd p4-change-delete (&rest args)
  "change"
  "Delete the changelist. This is only allowed if the pending changelist has no files or pending fixes."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 change" "" 'pending)
     (append (list "-d" (p4-completing-read 'pending "Changelist: ")))))
  (p4-call-command "change" args :mode 'p4-basic-list-mode))

; TODO All these should call:`p4 -ztag -F "%change%" opened | uniq` and then split by line
; to provide the completion candidates.
; 
(defp4cmd p4-revert-changelist (&rest args)
  "revert"
  "Reverts only the files in the specified changelist."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 revert" "" 'pending)
     (append (list "-c" (p4-completing-read 'pending "Changelist: ")) '("//...") )))
  (p4-call-command "revert" args :mode 'p4-basic-list-mode
                   :callback (p4-refresh-callback)))

(defp4cmd p4-revert-changelist-but-keep-workspace-files (&rest args)
  "revert"
  "Reverts only the files in the specified changelist, while preserving local workspace changes."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 revert" "" 'pending)
     (append (list "-k" "-c" (p4-completing-read 'pending "Changelist: ")) '("//...") )))
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

(defp4cmd p4-show-files-for-pending-changelist (&rest args)
  "files"
  "List files and display their status for a specific changelist."
  (interactive
    (if current-prefix-arg
       (p4-read-args "p4 files" "" 'pending)
       (append (list (concat "@=" (p4-completing-read 'pending "Changelist: "))))))
   (p4-call-command "files" args :mode 'p4-opened-list-mode
     :callback (lambda ()
                 (p4-regexp-create-links "\\<change \\([1-9][0-9]*\\) ([a-z]+)"
                                       'pending "Edit change"))
     :pop-up-output (lambda () t)))

(defp4cmd p4-show-files-for-shelved-changelist (&rest args)
  "files"
  "List shelved files and display their status for a specific changelist."
  (interactive
    (if current-prefix-arg
       (p4-read-args "p4 files" "" 'shelved)
       (append (list (concat "@=" (p4-completing-read 'shelved "Changelist: "))))))
   (p4-call-command "files" args :mode 'p4-opened-list-mode
     :callback (lambda ()
                 (p4-regexp-create-links "\\<change \\([1-9][0-9]*\\) ([a-z]+)"
                                       'shelved "Edit change"))
     :pop-up-output (lambda () t)))

(defp4cmd p4-show-files-for-submitted-changelist (&rest args)
  "files"
  "List submitted files and display their status for a specific changelist."
  (interactive
    (if current-prefix-arg
       (p4-read-args "p4 files" "" 'submitted)
       (append (list (concat "@=" (p4-completing-read 'submitted "Changelist: "))))))
   (p4-call-command "files" args :mode 'p4-opened-list-mode
     :callback (lambda ()
                 (p4-regexp-create-links "\\<change \\([1-9][0-9]*\\) ([a-z]+)"
                                       'submitted "Edit change"))
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
  (p4-call-command "unshelve" args :mode 'p4-basic-list-mode :callback (p4-refresh-callback)))

(defp4cmd p4-submit-shelved-changelist (&rest args)
  "submit"
  "Submits a previously-shelved changelist to the server."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 submit: " "" 'shelved)
     (append (list "-e" (p4-completing-read 'shelved "Shelved changelist: ")))))
  (p4-call-command "submit" args :mode 'p4-basic-list-mode :callback (p4-refresh-callback)))

(defp4cmd p4-submit-changelist (&rest args)
  "submit"
  "Submits a pending changelist to the server."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 submit: " "" 'pending)
     (append (list "-c" (p4-completing-read 'pending "Changelist: ")))))
  (p4-call-command "submit" args :mode 'p4-basic-list-mode :callback (p4-refresh-callback)))

; TODO: Allow setting stream to check in.
; Command is `p4 changes ...@30312822,30313050 -s submitted`
(defun p4-list-changes-between-changelists (&rest args)
  "Lists out the changes between two changelist numbers. Useful for bisecting or figuring out what changes might have triggered an issue."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 list-changes-between-changelists: " "" 'submitted)
     (let ((client-root (string-trim-right (shell-command-to-string "p4 -F %clientRoot% -ztag info"))))
       (list "-m" "200000" "-s" "submitted" (format "%s/...@%s,%s" client-root (p4-completing-read 'submitted "First CL #: ") (p4-completing-read 'submitted "Second CL #: "))))))
  (p4-call-command "changes" args :mode 'p4-basic-list-mode))

(defalias 'p4-sync-file 'p4-refresh)

; todo this isn't fully fleshed out yet.
(defun p4-submit-swarm-review (&rest args)
  "Submits a Swarm review for the given changelist."
  (interactive
  (let* ((changelist (p4-completing-read 'pending "Changelist: "))
         (swarm-url (getenv "P4SWARMURL"))  ; Environment variable for Swarm URL
         (p4-user (or (getenv "P4USER") "default_username"))  ; P4USER or default username
         (p4-ticket (shell-command-to-string "p4 login -s | awk '{print $2}' | tr -d '\n'"))  ; Get the current Perforce ticket
         (review-id nil)
         (reviewers (read-string "Enter reviewers (space-separated): "))
         (groups (read-string "Enter groups (space-separated): ")))

    ;; Ensure required environment variables are set
    (unless (and swarm-url p4-user p4-ticket)
      (error "Missing required environment variable(s). Please set `P4SWARMURL`, `P4USER`, and ensure you are logged in using 'p4 login'."))

    ;; Step 1: Create a Swarm review
    (with-current-buffer
        (url-retrieve-synchronously
         (format "%s/api/v10/reviews" swarm-url)
         (lambda (status)
           (goto-char (point-min))
           (search-forward-regexp "\n\n")
           (setq review-id (json-read))))
      (let ((data (json-encode `((changelist . ,changelist)
                                 (reviewers . ,(split-string reviewers))
                                 (groups . ,(split-string groups)))))
            (headers `(("Authorization" . ,(concat "Bearer " p4-ticket))
                       ("Content-Type" . "application/json"))))

        ;; Step 2: Set review details
        (url-retrieve
         (format "%s/api/v10/reviews/%s" swarm-url review-id)
         (lambda (status)
           (url-insert-file-contents
            (format "%s/api/v10/reviews/%s" swarm-url review-id))
           (url-http-parse-response)
           (let ((json-object-type 'plist))
             (setq review-details (json-read)))
           (setq review-details (plist-put review-details :state "approved"))
           (setq review-details (plist-put review-details :fields `((test-field . "test-value"))))

           ;; Step 3: Submit the review
           (url-retrieve
            (format "%s/api/v10/reviews/%s/submit" swarm-url review-id)
            (lambda (status)
              (if (= (url-http-parse-response) 200)
                  (message "Swarm review submitted: %s%s" swarm-url review-id)
                (message "Failed to submit Swarm review. Status code: %s" status))
              (kill-buffer))))))))))
;; Example usage:
;; (p4-submit-swarm-review <cl number>)

(defun p4-open-swarm-for-changelist ()
  "Opens the Swarm URL for the given changelist."
  (interactive)
  (let* ((cl (p4-completing-read 'shelved "Changelist: "))
         (swarm-url (getenv "P4SWARMURL")))
    (browse-url (concat swarm-url
                        "/changes/"
                        (url-hexify-string cl)))))

(defun p4-open-timelapse-view-of-file (file-path)
  "Run p4vc timelapse on a given path."
  (interactive
   (list (read-string "Enter path: " (buffer-file-name))))
  (let ((process-name "p4vc-timelapse-process")
        (command "p4vc")
        (arguments (list "timelapse" file-path)))
    (apply #'start-process process-name nil command arguments)
    (message "Started p4vc timelapse on: %s" file-path)))

(defun p4-open-revision-graph-view-of-file (file-path)
  "Run p4vc revisiongraph on a given path."
  (interactive
   (list (read-string "Enter path: " (buffer-file-name))))
  (let ((process-name "p4vc-revisiongraph-process")
        (command "p4vc")
        (arguments (list "revisiongraph" file-path)))
    (apply #'start-process process-name nil command arguments)
    (message "Started p4vc revisiongraph on: %s" file-path)))

; To compare two specific revisions alone, give a prefix argument to `p4-ediff`.
(defun p4-ediff-latest ()
  "Use ediff to compare file with the latest revision."
  (interactive)
  (p4-call-command "print" (list (concat (p4-context-single-filename) "#head"))
                   :after-show (p4-activate-ediff-callback)))

(defun p4-ediff-against (revision)
  "Use ediff to compare the current file against a user-specified revision."
  (interactive "sEnter revision (defaults to #head): ")
  (when (string= revision "")
    (setq revision "#head"))
  (p4-call-command "print"
                   (list (concat (p4-context-single-filename) revision))
                   :after-show (p4-activate-ediff-callback)))

; TODO: make a command that gets the latest CL description that modified a given line in a source file.
; TODO: make a command that allows modifying the description of a given changelist.
; TODO: make a command that allows using show files in shelved changelist to diff2 between the depot revision and the revision in the shelf,
; and also to diff against the current revision locally. Look at the current revision number, then look at the changelist number, print the two out to some buffer, and ediff those.

; TODO Stuff to implement.
;; (defp4cmd p4-unshelve-file (&rest args)
;;   "unshelve"
;;   "Restore a single shelved file from a pending change into a workspace."
;;   (interactive
;;    (if current-prefix-arg
;;        (p4-read-args "p4 unshelve: " "" 'shelved)
;;      (append (list "-f" "-s" (p4-completing-read 'shelved "Unshelve from: "))
;;              (when p4-open-in-changelist
;;                (list "-c" (p4-completing-read 'pending "Open in change: "))))))
;;   (p4-call-command "unshelve" args :mode 'p4-basic-list-mode))

; Implement a mode in the `p4-opened` map that allows bringing up emacs's ediff and also working
; for CLs that you don't own - i.e. you don't have the files currently open for edit.

; TODO write an interface to `p4 integrated` and `p4 filelog -i` for viewing revision graph history in pure text form.

; TODO p4-print-changelist-client-and-depot-versions
; TODO p4-print-file-client-and-depot-versions

; TODO p4-show-opened-changelists
; p4 -ztag -F %change% opened and remove duplicate lines

; TODO in general format all commands with have and server revisions
; p4 -ztag opened put `ztag` to see what can be used for format arguments.
; p4 -ztag -F "%change%" opened | uniq | p4 -x - -ztag -F "%change% %desc%" describe -s

; TODO make a command that can safely backup a given CL to a new one.
; TODO make shelve command fail if it tries to shelve empty so that it stops overwriting shelves.
; TODO make a patching utillity that uses
; p4 describe -du -S <CL number here> | sed -Ee 's|==== //(.*)#[0-9]+(.*)|+++ \1\n--- \1|' | awk '/^+++ /{f=1}f'

; TODO Make megapatch generation command
; p4 -Ztag -F %change% changes -m 10000 -s submitted //Fortnite/Release-35.00/...@41406195,41440259 | p4 -x - describe -du -S

(defun p4-create-swarm-review (reviewers groups)
  "TODO Create a Swarm review."
  
  )


(provide 'p4-extensions)
