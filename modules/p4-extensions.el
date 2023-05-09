(require 'p4)

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
       (p4-read-args "p4 opened:" "" 'shelved)
     (append (list "%depotFile%" "opened" "-c" (p4-completing-read 'shelved "Changelist: ")))))
  (p4-call-command "-F" args :mode 'p4-basic-list-mode))

; Command is `p4 -F %depotFile% opened -c 1234 | p4 -x - reopen -c 5678`
; to move files from 1234 to 5678
; TODO this executes, but doesn't actually work and doesn't move the files for some reason. The command after the pipe
; is somehow not getting executed for some reason.
(defp4cmd p4-move-files-from-changelist (&rest args)
  "move-files-from-changelist"
  "Moves files between changelists."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 move-files-from-changelist:" "" 'shelved)
     (append (list "%depotFile%" "opened" "-c" (p4-completing-read 'shelved "Move files from: ") "|" "p4" "-x" "-" "reopen" "-c")
             (when p4-open-in-changelist
               (list (p4-completing-read 'pending "New/existing shelf: "))))))
  (p4-call-command "-F" args :mode 'p4-basic-list-mode
                   :callback (p4-refresh-callback)))

(defp4cmd p4-shelve-force (&rest args)
  "shelve"
  "Store files (or a stream spec) from a pending changelist in the depot, without submitting them."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 shelve" "" 'shelved)
     (append (list "-p" "-r" "-c" (p4-completing-read 'shelved "Changelist: ")))))
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
       (p4-read-args "p4 change" "" 'shelved)
     (append (list "-d" (p4-completing-read 'shelved "Changelist: ")))))
  (p4-call-command "change" args :mode 'p4-basic-list-mode))

(defp4cmd p4-revert-changelist (&rest args)
  "revert"
  "Reverts only those files in the specified changelist."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 revert" "" 'shelved)
     (append (list "-c" (p4-completing-read 'shelved "Changelist: ")) '("//...") )))
  (p4-call-command "revert" args :mode 'p4-basic-list-mode
                   :callback (p4-refresh-callback)))

(defp4cmd p4-revert-changelist-and-wipe (&rest args)
  "revert"
  "Reverts only those files in the specified changelist. Also deletes files marked for add."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 revert" "" 'shelved)
     (append (list "-w" "-c" (p4-completing-read 'shelved "Changelist: ")) '("//...") )))
  (p4-call-command "revert" args :mode 'p4-basic-list-mode
                   :callback (p4-refresh-callback)))

(defp4cmd* changes-own-for-current-workspace
  "Shows your shelved changes (up to 200) for the current client workspace."
  (list "-m" "200" "--me" "-L" "-s" "shelved" "-c" (p4-current-client))
  (p4-file-change-log "changes" args))

(provide 'p4-extensions)
