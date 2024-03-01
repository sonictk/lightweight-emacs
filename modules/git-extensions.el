(defun ediff-patch-file (patch-file)
  "Open a unified patch file in ediff-mode with a side-by-side view of the diffs."
  (interactive "fPatch file: ")
  ;; Generate the before and after buffers from the patch file
  (let* ((before-buffer (generate-new-buffer "*patch-before*"))
         (after-buffer (generate-new-buffer "*patch-after*"))
         (patch-buffer (find-file-noselect patch-file)))
    ;; Parse the patch file and fill before and after buffers
    (with-current-buffer patch-buffer
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (cond ((string-match "^\\+" line)
                 (with-current-buffer after-buffer
                   (insert (substring line 1) "\n")))
                ((string-match "^\\-" line)
                 (with-current-buffer before-buffer
                   (insert (substring line 1) "\n")))
                ;; Handle context lines present in both before and after
                ((not (string-match "^\\(---\\|+++\\|@@\\)" line))
                 (with-current-buffer before-buffer
                   (insert line "\n"))
                 (with-current-buffer after-buffer
                   (insert line "\n")))))
        (forward-line 1)))
    ;; Setup cleanup hook for buffers
    ;(let ((cleanup-hook (lambda ()
    ;                      (when (buffer-live-p before-buffer) (kill-buffer before-buffer))
    ;                      (when (buffer-live-p after-buffer) (kill-buffer after-buffer))
    ;                      (when (buffer-live-p patch-buffer) (kill-buffer patch-buffer)))))
    ;  (add-hook 'ediff-cleanup-hook cleanup-hook))
    ;; Now open ediff with these buffers
    (ediff-buffers before-buffer after-buffer)))

(provide 'git-extensions)
