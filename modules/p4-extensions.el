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
          (p4-call-shell-command (list "-Ztag" "-F" "%depotFile%" (concat "@" (p4-completing-read 'pending "Sync to changelist: ")) "opened" (concat "-c " (p4-completing-read 'submitted "Changelist: "))  "|"
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

(defun p4--changelist-description (cl)
  "Return the Description field of changelist CL (trimmed)."
  (with-temp-buffer
    (let ((exit (call-process (p4-executable) nil t nil
                              "-ztag" "-F" "%Description%"
                              "describe" "-s" cl)))
      (unless (zerop exit)
        (error "p4 describe failed for CL %s: %s" cl (buffer-string))))
    (string-trim (buffer-string))))

(defun p4-backup-shelf (source-cl)
  "Make a backup copy of an existing shelved changelist SOURCE-CL.
Creates a new pending changelist with the same description prefixed by
\"[BACKUP] \", then copies SOURCE-CL's shelved files into it via
`p4 reshelve'.  Prompts with completion over your shelved CLs."
  (interactive
   (list (p4-completing-read 'shelved "Backup shelf (source CL): ")))
  (let* ((desc     (p4--changelist-description source-cl))
         (new-desc (concat "[BACKUP] " (if (string-empty-p desc) "(no description)" desc)))
         (form     (with-temp-buffer
                     (let ((exit (call-process (p4-executable) nil t nil
                                               "--field" (concat "Description=" new-desc)
                                               "--field" "Files="
                                               "change" "-o")))
                       (unless (zerop exit)
                         (error "p4 change -o failed: %s" (buffer-string))))
                     (buffer-string)))
         (result   (with-temp-buffer
                     (let ((exit (call-process-region
                                  form nil
                                  (p4-executable) nil t nil
                                  "change" "-i")))
                       (unless (zerop exit)
                         (error "p4 change -i failed: %s" (buffer-string))))
                     (buffer-string))))
    (if (string-match "Change \\([0-9]+\\) created" result)
        (let ((new-cl (match-string 1 result)))
          (message "Created backup CL %s; copying shelved files from %s..." new-cl source-cl)
          (p4-call-command "reshelve"
                           (list "-p" "-s" source-cl "-c" new-cl)
                           :mode 'p4-basic-list-mode
                           :callback (p4-refresh-callback)))
      (error "Could not parse new CL number from p4 output: %s" result))))

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

(defun p4--changelist-opened-files (cl)
  "Return the depot paths currently open in pending changelist CL.
Returns NIL when nothing is open in CL (`p4 opened' fails in that case)."
  (p4-output-matches (list "-ztag" "-F" "%depotFile%" "opened" "-c" cl)
                     "^//.+$"))

(defun p4--changelist-shelved-files (cl)
  "Return the depot paths shelved in changelist CL.
Returns NIL when CL has no shelf (`p4 files @=CL' fails in that case)."
  (p4-output-matches (list "-ztag" "-F" "%depotFile%" "files" (concat "@=" cl))
                     "^//.+$"))

(defun p4--changelist-arg (args)
  "Return the changelist number given as \"-c CL\" in ARGS, or NIL."
  (let ((tail (member "-c" args)))
    (when (and tail (cadr tail) (string-match-p "\\`[0-9]+\\'" (cadr tail)))
      (cadr tail))))

(defun p4--confirm-destructive-shelve (args)
  "Confirm a `p4 shelve' invocation described by ARGS that would empty a shelf.
`p4 shelve -r' replaces the shelf with whatever is open in the changelist, so
running it on a changelist with no open files wipes out an existing shelf with
no warning from the server.  Signal a `user-error' if the user declines."
  (let ((cl (p4--changelist-arg args)))
    (when (and cl (or (member "-r" args) (member "-f" args)))
      (let ((shelved (p4--changelist-shelved-files cl)))
        (when (and shelved (null (p4--changelist-opened-files cl)))
          ;; Logged so the full list survives in *Messages* after the prompt.
          (message "Shelf for CL %s currently holds:\n%s"
                   cl (mapconcat #'identity shelved "\n"))
          (unless (yes-or-no-p
                   (format "CL %s has no open files but %d shelved file(s); shelving now DELETES that shelf (see *Messages*).  Proceed? "
                           cl (length shelved)))
            (user-error "Aborted: shelf for CL %s left untouched" cl)))))))

(defp4cmd p4-shelve-force (&rest args)
  "shelve"
  "Store files (or a stream spec) from a pending changelist in the depot, without submitting them.
Asks for confirmation first when the changelist has an existing shelf but no
open files, since the `-r' replace would silently delete that shelf."
  (interactive
   (if current-prefix-arg
       (p4-read-args "p4 shelve" "" 'pending)
     (append (list "-p" "-r" "-c" (p4-completing-read 'pending "Changelist: ")))))
  (p4--confirm-destructive-shelve args)
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

(defun p4--fetch-stream-list ()
  "Return a list of depot stream paths from `p4 streams'."
  (split-string
   (string-trim (shell-command-to-string "p4 -ztag -F \"%Stream%\" streams -m 1000"))
   "\n" t "[ \t]+"))

(defvar p4--max-changes-history nil
  "Minibuffer history of maximum change counts.")

(defun p4--read-max-changes (&optional default)
  "Read a positive maximum change count, pre-filled with DEFAULT (200 if omitted).
A numeric prefix argument is used directly without prompting."
  (let ((default (or default 200))
        (n nil))
    (if current-prefix-arg
        (prefix-numeric-value current-prefix-arg)
      (while (progn
               (setq n (string-to-number
                        (string-trim
                         (read-from-minibuffer
                          "Max changes: " (number-to-string default)
                          nil nil 'p4--max-changes-history))))
               (< n 1))
        (message "Enter a positive number of changes")
        (sit-for 1))
      n)))

(defun p4-show-recent-changes-for-user (user stream status max-changes)
  "Show recent Perforce changes by USER on STREAM filtered by STATUS.
MAX-CHANGES limits results; the prompt is pre-filled with 200, and a numeric
prefix argument overrides the prompt entirely.
Changes are shown regardless of which client workspace they were submitted from."
  (interactive
   (let* ((user   (p4-completing-read 'user "User: "))
          (stream (completing-read "Stream: " (p4--fetch-stream-list) nil nil))
          (status (completing-read "Status: "
                                   '("submitted" "pending" "shelved")
                                   nil t nil nil "submitted"))
          (max    (p4--read-max-changes)))
     (list user stream status max)))
  (p4-file-change-log "changes"
                      (list "-L" "-t"
                            "-m" (number-to-string max-changes)
                            "-u" user
                            "-s" status
                            (concat stream "/..."))))

(defvar-local p4-changes-list--line-cl nil
  "Hash table mapping buffer line numbers to changelist number strings.")

(defun p4-changes-list--cl-at-point ()
  "Return the changelist number string for the line at point, or nil."
  (and p4-changes-list--line-cl
       (gethash (line-number-at-pos) p4-changes-list--line-cl)))

(defun p4-changes-list-describe-at-point ()
  "Run `p4 describe' on the changelist at point and show the result."
  (interactive)
  (if-let ((cl (p4-changes-list--cl-at-point)))
      (p4-call-command "describe" (list cl))
    (message "No changelist at point")))

(defun p4-changes-list-patch-at-point ()
  "Generate a unified diff patch for the changelist at point."
  (interactive)
  (if-let ((cl (p4-changes-list--cl-at-point)))
      (p4-generate-patch-for-changelist cl 3)
    (message "No changelist at point")))

(defun p4-show-changes-by-status (stream status user max-changes)
  "Show Perforce changes on STREAM with STATUS submitted by USER.
With a numeric prefix argument, show that many changes (default 200).
Prompts for stream, status, and username with completion.

In the results buffer:
  RET or mouse-1  – run `p4 describe' on the changelist at point
  d               – generate a diff patch via `p4-generate-patch-for-changelist'
  q               – quit the window"
  (interactive
   (let* ((max    (if current-prefix-arg
                      (prefix-numeric-value current-prefix-arg)
                    200))
          (stream (completing-read "Stream: " (p4--fetch-stream-list) nil nil))
          (status (completing-read "Status: "
                                   '("submitted" "pending" "shelved")
                                   nil t nil nil "submitted"))
          (user   (p4-completing-read 'user "User: ")))
     (list stream status user max)))
  (message "Fetching %s changes for %s on %s..." status user stream)
  (let* ((raw   (string-trim
                 (shell-command-to-string
                  (format "p4 changes -m %d -s %s -u %s %s/..."
                          max-changes
                          (shell-quote-argument status)
                          (shell-quote-argument user)
                          (shell-quote-argument stream)))))
         (lines (seq-filter (lambda (l) (string-match-p "^Change " l))
                            (split-string raw "\n" t)))
         (buf   (get-buffer-create
                 (format "*P4 Changes: %s — %s — %s*" user status stream))))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (lm (make-hash-table :test 'eql)))
        (erase-buffer)
        (special-mode)
        (setq-local p4-changes-list--line-cl lm)
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map special-mode-map)
          (define-key map (kbd "RET") #'p4-changes-list-describe-at-point)
          (define-key map (kbd "d")   #'p4-changes-list-patch-at-point)
          (define-key map (kbd "q")   #'quit-window)
          (use-local-map map))
        (insert (format "%d %s change(s) for %s on %s (max %d)\n\n"
                        (length lines) status user stream max-changes))
        (if (null lines)
            (insert "(no results)\n")
          (dolist (line lines)
            (when (string-match
                   "^Change \\([0-9]+\\) on \\([^ ]+\\) by \\([^@]+\\)@[^ ]+ '\\(.*\\)'$"
                   line)
              (let* ((cl   (match-string 1 line))
                     (date (match-string 2 line))
                     (usr  (match-string 3 line))
                     (desc (match-string 4 line))
                     (lnum (line-number-at-pos)))
                (puthash lnum cl lm)
                (insert-text-button
                 cl
                 'face 'compilation-info
                 'follow-link t
                 'mouse-face 'highlight
                 'help-echo "RET/mouse-1: p4 describe  |  d: patch"
                 'action (lambda (_b) (p4-changes-list-describe-at-point)))
                (insert (format "  %s  %-14s  %s\n" date usr desc))))))
        (goto-char (point-min))
        (set-buffer-modified-p nil)))
    (switch-to-buffer-other-window buf)
    (message "Done.")))

;;; ---------------------------------------------------------------------------
;;; p4-blame-range / p4-blame-range-diff
;;;
;;; Show every submitted change on a stream within a CL range, equivalent to:
;;;   p4 -Ztag -F %change% changes -m 1000000 -s submitted STREAM/...@A,@B \
;;;     | p4 -x - describe -du -s     (p4-blame-range)
;;;   p4 -Ztag -F %change% changes -m 1000000 -s submitted STREAM/...@A,@B \
;;;     | p4 -x - describe -du -S     (p4-blame-range-diff, includes diffs)
;;;
;;; The full piped output is parsed into per-CL `describe' blocks.  Each line
;;; in the list buffer shows the CL and a 250-char description; the rest of
;;; the block is inserted as invisible text on the following lines, so isearch
;;; transparently matches descriptions, file paths, diff hunks, etc., just as
;;; if you were searching the raw command output.

(defvar-local p4-blame-range--blocks nil
  "Alist of (CL . BLOCK-STRING) for the current blame-range buffer.")
(defvar-local p4-blame-range--include-diffs nil
  "Whether describe blocks in this buffer include diff hunks.")

(defun p4--blame-range-clean-cl (cl)
  "Strip leading @ and surrounding whitespace from CL."
  (let ((s (string-trim cl)))
    (if (string-prefix-p "@" s) (substring s 1) s)))

(defun p4--blame-range-run (stream start-cl end-cl describe-flags)
  "Run the piped blame-range command and return the full output as a string.
DESCRIBE-FLAGS is the trailing string of flags passed to `p4 describe',
e.g. \"-du -s\" or \"-du -S\"."
  (shell-command-to-string
   (format "p4 -Ztag -F %%change%% changes -m 1000000 -s submitted %s/...@%s,@%s | p4 -x - describe %s"
           (shell-quote-argument stream)
           (shell-quote-argument start-cl)
           (shell-quote-argument end-cl)
           describe-flags)))

(defun p4--blame-range-parse-blocks (output)
  "Parse OUTPUT into an alist of (CL . BLOCK-TEXT) in source order."
  (let (blocks)
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (re-search-forward "^Change \\([0-9]+\\) " nil t)
        (let* ((cl    (match-string 1))
               (start (line-beginning-position))
               (end   (save-excursion
                        (forward-line 1)
                        (if (re-search-forward "^Change [0-9]+ " nil t)
                            (line-beginning-position)
                          (point-max)))))
          (push (cons cl (buffer-substring-no-properties start end)) blocks)
          (goto-char end))))
    (nreverse blocks)))

(defun p4--blame-range-extract-desc (block)
  "Extract the description portion of a describe BLOCK joined onto one line."
  (with-temp-buffer
    (insert block)
    (goto-char (point-min))
    (forward-line 1)                                       ; past "Change ..." header
    (while (and (not (eobp)) (looking-at "^[ \t]*$"))      ; skip blank lines
      (forward-line 1))
    (let (lines)
      (while (and (not (eobp))
                  (looking-at "^[ \t]+"))                  ; indented description lines
        (push (string-trim
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
              lines)
        (forward-line 1))
      (mapconcat #'identity (nreverse lines) " "))))

(defun p4-blame-range-show-at-point ()
  "Show the full `p4 describe' block for the CL at point in a new buffer."
  (interactive)
  (let* ((cl    (get-text-property (point) 'p4-blame-cl))
         (block (and cl (cdr (assoc cl p4-blame-range--blocks)))))
    (cond
     ((not cl)    (message "No changelist at point"))
     ((not block) (message "No describe data for CL %s" cl))
     (t
      (let ((buf (get-buffer-create (format "*P4 Describe: %s*" cl))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert block)
            (goto-char (point-min))
            (diff-mode)
            (read-only-mode 1)
            (local-set-key (kbd "q") #'quit-window)))
        (switch-to-buffer-other-window buf))))))

(defun p4--blame-range-display (stream start end output include-diffs)
  "Render the parsed list buffer for the blame-range command."
  (let* ((blocks (p4--blame-range-parse-blocks output))
         (buf    (get-buffer-create
                  (format "*P4 Blame Range%s: %s @%s,@%s*"
                          (if include-diffs " (diff)" "")
                          stream start end))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (setq-local p4-blame-range--blocks blocks)
        (setq-local p4-blame-range--include-diffs include-diffs)
        (setq-local buffer-invisibility-spec '(p4-blame-block))
        (setq-local search-invisible 'open)
        (setq-local line-move-ignore-invisible t)
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map special-mode-map)
          (define-key map (kbd "RET") #'p4-blame-range-show-at-point)
          (define-key map (kbd "q")   #'quit-window)
          (use-local-map map))
        (insert (format "Stream: %s   Range: @%s,@%s   %d change(s)%s\n"
                        stream start end (length blocks)
                        (if include-diffs "   [diffs included]" "")))
        (insert "RET on a CL → describe.  isearch sees the full underlying output.\n\n")
        (if (null blocks)
            (insert "(no results)\n")
          (dolist (b blocks)
            (let* ((cl     (car b))
                   (block  (cdr b))
                   (dfull  (p4--blame-range-extract-desc block))
                   (desc   (if (> (length dfull) 250)
                               (concat (substring dfull 0 247) "...")
                             dfull))
                   (line-s (point)))
              (insert-text-button
               cl
               'face 'compilation-info
               'follow-link t
               'mouse-face 'highlight
               'help-echo "RET/mouse-1: show describe output"
               'action (lambda (_b) (p4-blame-range-show-at-point)))
              (insert (format "  %s\n" desc))
              (put-text-property line-s (point) 'p4-blame-cl cl)
              (let ((inv-s (point)))
                (insert block)
                (unless (eq (char-before) ?\n) (insert "\n"))
                (put-text-property inv-s (point) 'invisible 'p4-blame-block)
                (put-text-property inv-s (point) 'p4-blame-cl cl)))))
        (goto-char (point-min))
        (set-buffer-modified-p nil)))
    (switch-to-buffer-other-window buf)))

(defun p4--blame-range-prompt (include-diffs)
  "Prompt for stream + CL range, run the pipe, and display the result buffer."
  (let* ((stream (completing-read "Stream: " (p4--fetch-stream-list) nil nil))
         (start  (p4--blame-range-clean-cl (read-string "Start CL: ")))
         (end    (p4--blame-range-clean-cl (read-string "End CL: ")))
         (flags  (if include-diffs "-du -S" "-du -s")))
    (message "Running p4 blame-range for %s @%s,@%s..." stream start end)
    (let ((output (p4--blame-range-run stream start end flags)))
      (if (string-empty-p (string-trim output))
          (message "No output (no submitted changes in range?)")
        (p4--blame-range-display stream start end output include-diffs)
        (message "Done.")))))

(defun p4-blame-range ()
  "Show submitted changes on a stream within a CL range.

Prompts for STREAM (with completion), START CL, and END CL, then runs:
  p4 -Ztag -F %change% changes -m 1000000 -s submitted STREAM/...@START,@END
    | p4 -x - describe -du -s

Displays one line per change with CL on the left and the description
truncated to 250 chars on the right.  RET opens the full `p4 describe'
block (description + affected files) for the change at point in a new
buffer.

The full piped output is retained as invisible text in the buffer, so
isearch transparently matches against any portion of it (descriptions,
file paths, etc.) — same as searching the raw command output."
  (interactive)
  (p4--blame-range-prompt nil))

(defun p4-blame-range-diff ()
  "Like `p4-blame-range', but include unified diffs.

Prompts for STREAM, START CL, END CL, then runs:
  p4 -Ztag -F %change% changes -m 1000000 -s submitted STREAM/...@START,@END
    | p4 -x - describe -du -S

RET opens the full describe block (which now includes the unified diff
for the change) in a new buffer using `diff-mode' for highlighting."
  (interactive)
  (p4--blame-range-prompt t))

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

(defun p4--opened-changelists ()
  "Return a sorted list of distinct changelist numbers with files currently opened.
The string \"default\" represents the default changelist."
  (let (cls)
    (with-temp-buffer
      (let ((exit (call-process (p4-executable) nil t nil
                                "-ztag" "-F" "%change%" "opened")))
        (unless (zerop exit)
          (error "p4 opened failed: %s" (buffer-string))))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-trim (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))))
          (when (and (not (string-empty-p line))
                     (not (member line cls)))
            (push line cls)))
        (forward-line 1)))
    (sort cls
          (lambda (a b)
            (cond
             ((string= a "default") nil)
             ((string= b "default") t)
             (t (> (string-to-number a) (string-to-number b))))))))

(defun p4--populate-opened-changelist-annotations (cls)
  "Populate the global `p4-completion-annotations' for CLs in CLS.
Uses the same `p4 changes -l' format and regex that `p4-fetch-change-completions'
uses, so multi-line descriptions are handled correctly."
  (let ((ht     (make-hash-table :test 'equal))
        (client (p4-current-client)))
    (with-temp-buffer
      (let ((exit (apply #'call-process (p4-executable) nil t nil
                         (append (list "changes" "-m" "200" "-s" "pending" "-l")
                                 (when client (list "-c" client))))))
        (when (zerop exit)
          (goto-char (point-min))
          (while (re-search-forward "^Change \\([0-9]+\\) .*\n+\\(.*\\)\n" nil t)
            (let* ((cl (match-string 1))
                   (d  (string-trim (match-string 2))))
              (when (member cl cls)
                (puthash cl d ht)))))))
    (when (member "default" cls)
      (puthash "default" "(default changelist)" ht))
    (setq p4-completion-annotations ht)))

(defun p4-show-opened-changelists ()
  "Prompt with completion over the changelists you currently have files opened in.
After selection, invoke `p4-show-opened-for-changelist' to list that CL's files.
Uses the same annotation mechanism as `p4-completing-read', so descriptions
appear alongside each candidate."
  (interactive)
  (let ((cls (p4--opened-changelists)))
    (cond
     ((null cls)
      (message "No files currently opened."))
     (t
      (p4--populate-opened-changelist-annotations cls)
      (let* ((table
              (lambda (string pred action)
                (if (eq action 'metadata)
                    '(metadata
                      (category . p4-change)
                      (annotation-function . p4-completion-annotate))
                  (complete-with-action action cls string pred))))
             (completion-extra-properties
              '(:annotation-function p4-completion-annotate))
             (cl (completing-read "Opened changelist: " table nil t)))
        (when (and cl (not (string-empty-p cl)))
          (p4-show-opened-for-changelist "-c" cl)))))))

(defp4cmd p4-show-files-for-changelist (&rest args)
  "files"
  "List files and display their status for a specific changelist.
Prompts for the changelist type (pending, shelved or submitted) and then
for the changelist itself, with completion scoped to the chosen type."
  (interactive
    (if current-prefix-arg
       (p4-read-args "p4 files" "" 'pending)
       (let ((type (intern (completing-read "Changelist type: "
                                            '("pending" "shelved" "submitted")
                                            nil t))))
         (append (list (concat "@=" (p4-completing-read type "Changelist: ")))))))
   (p4-call-command "files" args :mode 'p4-opened-list-mode
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
; p4 describe -du -S <CL number here> | sed -Ee "s|==== //(.*)#[0-9]+(.*)|+++ \1\n--- \1|" | awk "/^+++ /{f=1}f"

; TODO Make megapatch generation command
; p4 -Ztag -F %change% changes -m 10000 -s submitted //Depot/Release-X.XX/...@41406195,41440259 | p4 -x - describe -du -S

(defun p4-get-changes-for-range (stream begin end diff-type)
  "Generate a Perforce \"megapatch\" of the changes between a changelist range."
  (interactive
   (let* ((stream (p4-completing-read 'streams "Stream to use: "))
          (begin (p4-completing-read 'submitted "Starting from changelist: "))
          (end (p4-completing-read 'submitted "Until changelist: "))
          ))
   (list begin end)
   )
  )

; This replaces needing to go to Swarm to see the diff of a changelist.
(defun p4-generate-patch-for-changelist (changelist-arg &optional context-lines-arg)
  "Generate a Perforce patch file from a specified changelist.

  When called interactively:
  - Prompts for the Perforce changelist number.
  - Optionally prompts for the number of context lines (default 3).

  When called non-interactively:
  - `CHANGELIST-ARG`: The changelist number (string).
  - `CONTEXT-LINES-ARG`: The number of context lines (integer, defaults to 3 if nil or 0).

  The output of 'p4 describe -du -S <CL#>' is processed and displayed
  in a new buffer named 'diff-<CL#>.patch'."
  (interactive
   (let* ((cl-num (p4-completing-read 'pending "Changelist: "))
          (context-lines-str (read-string "Context lines (default 3): " nil nil "3"))
          (context-lines (if (string-empty-p context-lines-str)
                             3
                           (string-to-number context-lines-str))))
     (list cl-num context-lines)))

  (let* ((changelist (or changelist-arg (error "Changelist number must be provided.")))
         (effective-context-lines (if (and context-lines-arg (> context-lines-arg 0))
                                      context-lines-arg
                                    3)) ;; Default to 3 if arg is nil or <= 0
         (buffer-name (format "*diff-%s.patch*" changelist))
         (command (format "p4 describe -du%d -S %s | sed -Ee \"s|==== //(.*)#[0-9]+(.*)|+++ \\1\\n--- \\1|\" | awk \"/^+++ /{f=1}f\""
                          effective-context-lines changelist))
         patch-content)

    (message "Generating patch for changelist %s with %d context lines..."
             changelist effective-context-lines)

    (setq patch-content (shell-command-to-string command))

    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert patch-content)
      (goto-char (point-min))
      (diff-mode)
      (view-mode)
      (setq-local compilation-read-only-buffer t) ; For compilation-mode derivatives
      (setq-local buffer-read-only t)             ; General read-only
      (display-buffer (current-buffer)))

    (when (string-empty-p patch-content)
      (message "No patch content generated for changelist %s. Check changelist number and P4 environment." changelist)
      (kill-buffer buffer-name) ;; Clean up empty buffer
      (error "Patch generation failed or resulted in empty content."))

    (message "Patch for changelist %s generated in buffer %s." changelist buffer-name)))

(defun p4-create-swarm-review (reviewers groups)
  "TODO Create a Swarm review."
  
  )

(defun p4-integ-changelist-to-branch (changelist from-branch to-branch)
  "Integrate files from a given CHANGELIST, changing FROM-BRANCH to TO-BRANCH in depot paths."
  (interactive "nChangelist number: \nsFrom branch prefix (e.g. //depot/dev/): \nsTo branch prefix (e.g. //depot/main/): ")
  (let* ((cmd (format "p4 -Ztag -F %%depotFile%% files @=%d" changelist))
         (files (split-string (shell-command-to-string cmd) "\n" t))
         (pairs (mapcar (lambda (file)
                          (let ((target (replace-regexp-in-string
                                         (regexp-quote from-branch) to-branch file)))
                            (format "p4 integ %s %s" file target)))
                        files)))
    (with-output-to-temp-buffer "*P4 Integ Output*"
      (dolist (cmd pairs)
        (princ (format "%s\n" cmd))
        (shell-command cmd)))))

(defun p4-user-insert-at-point ()
  "Prompt for a Perforce username with completion and insert it at point."
  (interactive)
  (let ((username (p4-completing-read 'user "Username: ")))
    (when (and username (not (string-empty-p username)))
      (insert username))))

(define-derived-mode p4-revision-graph-mode p4-basic-mode "P4 Rev Graph"
  "Major mode for displaying a Perforce revision graph.")

(defun p4--rgraph-parse (output)
  "Parse p4 filelog -i -l OUTPUT into an alist of (FILE-PATH . REVISIONS).
Each revision is a plist: :rev :change :action :date :user :integrations.
Each integration is a plist: :direction (from|into) :action :path."
  (let (result cur-file cur-revs cur-rev cur-desc)
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ;; File header: bare depot path with no leading dots
           ((string-match "^\\(//[^ \t\n]+\\)$" line)
            (when cur-file
              (when cur-rev
                (setq cur-rev (plist-put cur-rev :desc cur-desc))
                (push cur-rev cur-revs))
              (push (cons cur-file (nreverse cur-revs)) result))
            (setq cur-file (match-string 1 line)
                  cur-revs nil cur-rev nil cur-desc ""))
           ;; Revision: ... #N change M action on DATE by USER@CLIENT
           ((string-match (concat "^\\.\\.\\. #\\([0-9]+\\) change \\([0-9]+\\) "
                                  "\\([a-z/]+\\) on \\([0-9/]+\\) by "
                                  "\\([^ @]+\\)@\\([^ \n]+\\)")
                          line)
            (when cur-rev
              (setq cur-rev (plist-put cur-rev :desc cur-desc))
              (push cur-rev cur-revs))
            (setq cur-rev (list :rev    (string-to-number (match-string 1 line))
                                :change (string-to-number (match-string 2 line))
                                :action (match-string 3 line)
                                :date   (match-string 4 line)
                                :user   (match-string 5 line)
                                :integrations nil)
                  cur-desc ""))
           ;; Integration: ... ... action from/into //depot/path#rev
           ((string-match "^\\.\\.\\. \\.\\.\\. \\([a-z]+\\) \\(from\\|into\\) \\(//[^ \t\n]+\\)" line)
            (when cur-rev
              (setq cur-rev
                    (plist-put cur-rev :integrations
                               (append (plist-get cur-rev :integrations)
                                       (list (list :direction (match-string 2 line)
                                                   :action    (match-string 1 line)
                                                   :path      (match-string 3 line))))))))
           ;; Description line (tab-indented, from -l flag)
           ((string-match "^\t\\(.*\\)" line)
            (when cur-rev
              (setq cur-desc (if (string-empty-p cur-desc)
                                 (match-string 1 line)
               (concat cur-desc "\n" (match-string 1 line)))))))
          (forward-line 1))))
    (when cur-file
      (when cur-rev
        (setq cur-rev (plist-put cur-rev :desc cur-desc))
        (push cur-rev cur-revs))
      (push (cons cur-file (nreverse cur-revs)) result))
    (nreverse result)))

(defun p4--rgraph-integ-col (path files)
  "Return the column index in FILES matching the depot path in PATH (strips revision spec)."
  (let ((file (replace-regexp-in-string "#[0-9,#]+$" "" path))
        (result nil) (i 0))
    (dolist (entry files result)
      (when (and (not result) (string= (car entry) file))
        (setq result i))
      (setq i (1+ i)))))

(defun p4--rgraph-track (ncols active-cols node-col src-col)
  "Build the ASCII track string for one graph row.
NCOLS total columns; ACTIVE-COLS is list of active indices; NODE-COL gets `*';
SRC-COL (optional) draws a horizontal arrow from SRC-COL to NODE-COL."
  (let ((v (make-string (max 1 (1- (* 2 ncols))) ?\s)))
    (dolist (c active-cols)
      (aset v (* 2 c) ?|))
    (when (and src-col node-col (/= src-col node-col))
      (let ((i (* 2 (min node-col src-col)))
            (end (* 2 (max node-col src-col))))
        (while (<= i end) (aset v i ?-) (setq i (1+ i))))
      (aset v (* 2 src-col) ?+))
    (when node-col (aset v (* 2 node-col) ?*))
    v))

(defun p4--rgraph-render (parsed-files)
  "Render PARSED-FILES as a multi-column revision graph into the current buffer."
  (let* ((ncols (length parsed-files))
         (col-ranges (make-vector ncols nil))
         all-nodes)
    ;; Per-column change ranges (min . max)
    (dotimes (ci ncols)
      (let ((changes (mapcar (lambda (r) (plist-get r :change))
                             (cdr (nth ci parsed-files)))))
        (when changes
          (aset col-ranges ci (cons (apply #'min changes) (apply #'max changes))))))
    ;; Header legend
    (dotimes (ci ncols)
      (insert (propertize (format "  [%d] %s\n" ci (car (nth ci parsed-files)))
                          'face 'p4-filespec-face)))
    (insert "\n")
    ;; Collect all nodes with column index, sort by change descending
    (dotimes (ci ncols)
      (dolist (rev (cdr (nth ci parsed-files)))
        (push (cons ci rev) all-nodes)))
    (setq all-nodes
          (sort all-nodes
                (lambda (a b) (> (plist-get (cdr a) :change)
                                 (plist-get (cdr b) :change)))))
    ;; Render
    (let ((first-row t))
      (dolist (entry all-nodes)
        (let* ((ci     (car entry))
               (rev    (cdr entry))
               (change (plist-get rev :change))
               (integs (plist-get rev :integrations))
               ;; Columns whose change range spans this change number
               (active (let (acc)
                         (dotimes (k ncols)
                           (let ((r (aref col-ranges k)))
                             (when (and r (<= (car r) change) (<= change (cdr r)))
                               (push k acc))))
                         (nreverse acc)))
               ;; First integration resolvable to a known column
               (primary (let (found)
                          (dolist (ig integs)
                            (when (and (not found)
                                       (p4--rgraph-integ-col
                                        (plist-get ig :path) parsed-files))
                              (setq found ig)))
                          found))
               (src-col (when primary
                          (p4--rgraph-integ-col
                           (plist-get primary :path) parsed-files)))
               (track-width (max 1 (1- (* 2 ncols)))))
          ;; Separator pipe row (skip before very first node)
          (unless first-row
            (insert (p4--rgraph-track ncols active nil nil) "\n"))
          (setq first-row nil)
          ;; Node row
          (insert (p4--rgraph-track ncols active ci src-col))
          (insert (format "   ")
          (insert (propertize (number-to-string change) 'face 'p4-change-face))
          (insert (format "  #%-3d %-10s  %s  "
                          (plist-get rev :rev)
                          (plist-get rev :action)
                          (plist-get rev :date)))
          (insert (propertize (plist-get rev :user) 'face 'p4-user-face))
          ;; Integration annotations
          (dolist (ig integs)
            (insert (format "\n%s  %s %s "
                            (make-string (+ 3 track-width) ?\s)
                            (if (string= (plist-get ig :direction) "from") "<<" ">>")
                            (plist-get ig :action)))
            (insert (propertize (plist-get ig :path) 'face 'p4-filespec-face)))
          (insert "\n")))))))

(defun p4--rgraph-activate ()
  "Parse raw filelog output in current buffer and re-render as a graph."
  (let ((raw (buffer-string))
        (inhibit-read-only t))
    (erase-buffer)
    (p4--rgraph-render (p4--rgraph-parse raw))))

(defun p4-revision-graph ()
  "Display a text revision graph for the current file, following integrations across streams."
  (interactive)
  (p4-call-command "filelog"
                   (list "-i" "-l" (p4-context-single-filename))
                   :mode 'p4-revision-graph-mode
                   :callback 'p4--rgraph-activate))

;; describe -s 42024482
;; change -o 42024482
;; fstat -Op -Rs -e 42024482 //...
;; describe -s 42024482
;; fstat -Olhp //Depot/Main/Path/To/SomeFile.cpp
;; fstat -Olhp //Depot/Main/Path/To/SomeFile.cpp
;; fstat -OL -L //Depot/Main/Path/To/SomeFile.cpp#6
;; fstat -Olp //Depot/Main/Path/To/SomeFile.cpp#6
;; fstat -OL -L //Depot/Main/Path/To/SomeFile.cpp#6
;; fstat -Olp //Depot/Main/Path/To/SomeFile.cpp@=42024482
;; diff2 //Depot/Main/Path/To/SomeFile.cpp#6 //Depot/Main/Path/To/SomeFile.cpp@=42024482
;; print -o %TEMP%\p4v\<workspace>_<p4port-host>_<port>\Depot\Main\Path\To\SomeFile#6.cpp //Depot/Main/Path/To/SomeFile.cpp#6
;; print -o %TEMP%\p4v\<workspace>_<p4port-host>_<port>\Depot\Main\Path\To\SomeFile@=42024482.cpp //Depot/Main/Path/To/SomeFile.cpp@=42024482
;; 150018eb] //Depot/Main/Path/To/SomeFile.cpp#6 - edit change 41956326 (text)
;; 150018eb] //Depot/Main/Path/To/SomeFile.cpp#6 - edit change 42024482 (text)

(provide 'p4-extensions)
