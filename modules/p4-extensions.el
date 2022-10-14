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
  (p4-call-command "reshelve" args :mode 'p4-basic-list-mode))

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
     (append (list "-c" (p4-completing-read 'shelved "Changelist: ")))))
  (p4-call-command "revert" args :mode 'p4-basic-list-mode))

(provide 'p4-extensions)
