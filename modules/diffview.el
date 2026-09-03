;;; diffview.el --- View diffs in side-by-side format -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016 Free Software Foundation, Inc.

;; Author: Mitchel Humpherys <mitch.special@gmail.com>
;; Maintainer: Mitchel Humpherys <mitch.special@gmail.com>
;; Keywords: convenience, diff
;; Version: 1.0
;; URL: https://github.com/mgalgs/diffview-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Render a unified diff (top/bottom) in an easy-to-comprehend side-by-side
;; format.  This comes in handy for reading patches from mailing lists (or
;; from whencever you might acquire them).
;;
;;; Installation:
;;
;;     M-x package-install diffview
;;
;;; Usage:
;;
;; The following functions are provided for launching a side-by-side diff:
;;
;; o `diffview-current' : View the current diff buffer side-by-side
;; o `diffview-region' : View the current diff region side-by-side
;; o `diffview-message' : View the current email message (which presumably
;;    contains a patch) side-by-side
;;
;;; Keybindings
;;
;; o `}' : Next file
;; o `{' : Previous file
;; o `n' : Next change
;; o `p' : Previous change
;; o `l' : Align windows
;; o `q' : Quit
;;
;;; Screenshots:
;;
;; Before:
;; https://raw.github.com/mgalgs/diffview-mode/master/screenshots/diffview-before.png
;;
;; After:
;; https://raw.github.com/mgalgs/diffview-mode/master/screenshots/diffview-after.png
;;
;;; Code:

(require 'message)

(defun diffview--print-all-lines-to-buffer (lines buffer-name)
  "Prints each line in `LINES' to a buffer named `BUFFER-NAME'."
  (let ((old-temp-buffer (get-buffer buffer-name)))
    ;; (with-output-to-temp-buffer buffer-name
    (when old-temp-buffer
      (kill-buffer old-temp-buffer))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (dolist (line lines)
	(insert line "\n")))))

(defvar diffview--minus-bufname "*side-by-side-1*")
(defvar diffview--plus-bufname "*side-by-side-2*")
(defvar diffview--saved-wincfg nil)
(defvar diffview--regexp-is-plus-line "^\\+\\([^+]\\{1\\}\\|$\\)"
  "A + followed by one non + or the end of the line.")
(defvar diffview--regexp-is-minus-line "^-\\([^-]\\{1\\}\\|$\\)"
  "A - followed by one non - or the end of the line.")
(defvar diffview--changed-lines nil
  "Vector whose Nth element is non-NIL when line N+1 belongs to a change.")
(defvar diffview--changed-lines-key nil
  "Identity of the buffers `diffview--changed-lines' was computed from.")

(defun diffview--view-string (input-string)
  "Displays `INPUT-STRING' (a diff) in a side-by-side view."
  (setq diffview--saved-wincfg (current-window-configuration))
  (delete-other-windows)
  (let (plus-lines
	minus-lines
	tmp-line
	(current-state 'in-common)
	(last-state 'in-common)
	(current-lines-in-plus 0)
	(current-lines-in-minus 0)
	(total-lines 0)
	(all-lines (split-string input-string "\n")))
    (dolist (line all-lines)
      (cond
       ((string-match diffview--regexp-is-plus-line line)
	(push line plus-lines)
	(setq current-state 'in-plus)
	(setq current-lines-in-plus (1+ current-lines-in-plus)))
       ((string-match diffview--regexp-is-minus-line line)
	(push line minus-lines)
	(setq current-state 'in-minus)
	(setq current-lines-in-minus (1+ current-lines-in-minus)))
       ;; everything else must be common
       (t
	(push line plus-lines)
	(push line minus-lines)
	(setq current-state 'in-common)))

      (setq total-lines (1+ total-lines))

      ;; Process hunk state transitions
      (when (not (equal current-state last-state))
	;; there's been a state change
	(when (equal current-state 'in-common)
	  ;; we're transitioning out the +/- part of a hunk. We would
	  ;; like both sides to have the same number lines for this
	  ;; hunk, so we might need to fill one side or the other with
	  ;; empty lines.
	  (cond
	   ((> current-lines-in-plus current-lines-in-minus)
	    ;; need to fill minus
	    (setq tmp-line (pop minus-lines))
	    (dotimes (_ (- current-lines-in-plus current-lines-in-minus))
	      (push "" minus-lines))
	    (push tmp-line minus-lines))
	   ((< current-lines-in-plus current-lines-in-minus)
	    ;; need to fill plus
	    (setq tmp-line (pop plus-lines))
	    (dotimes (_ (- current-lines-in-minus current-lines-in-plus))
	      (push "" plus-lines))
	    (push tmp-line plus-lines)))

	  (setq current-lines-in-plus  0
		current-lines-in-minus 0)))

      (setq last-state current-state))

    (setq diffview--changed-lines nil
          diffview--changed-lines-key nil)

    (diffview--print-all-lines-to-buffer (reverse minus-lines) diffview--minus-bufname)
    (diffview--print-all-lines-to-buffer (reverse plus-lines) diffview--plus-bufname)

    (switch-to-buffer diffview--minus-bufname nil t)
    (goto-char (point-min))
    (diffview-mode)

    (split-window-right)
    (other-window 1)

    (switch-to-buffer diffview--plus-bufname nil t)
    (goto-char (point-min))
    (diffview-mode)

    (scroll-all-mode)))

;;;###autoload
(defun diffview-current ()
  "Show current diff buffer in a side-by-side view."
  (interactive)
  (diffview--view-string (buffer-string)))

;;;###autoload
(defun diffview-region ()
  "Show current diff region in a side-by-side view."
  (interactive)
  (diffview--view-string (buffer-substring (point) (mark))))

;;;###autoload
(defun diffview-message ()
  "Show `message-mode' buffer in a side-by-side view.

This is useful for reading patches from mailing lists."
  (interactive)
  (let (beg end)
    (save-excursion
      (message-goto-body)
      (search-forward-regexp "^---$")
      (setq beg (1+ (point)))
      (search-forward-regexp "^-- $")
      (setq end (1+ (point)))
      (diffview--view-string (buffer-substring beg end)))))

(defvar diffview--mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "l") 'diffview--align-windows)
    (define-key km (kbd "}") 'diffview--next-file)
    (define-key km (kbd "{") 'diffview--prev-file)
    (define-key km (kbd "n") 'diffview--next-change)
    (define-key km (kbd "p") 'diffview--prev-change)
    (define-key km (kbd "M-n") 'diffview--next-change)
    (define-key km (kbd "M-p") 'diffview--prev-change)
    (define-key km (kbd "q") 'diffview--quit)
    km)
  "Special keymap for `diffview--mode-map'.")

(easy-menu-define
  diffview--menu diffview--mode-map "diffview menu"
  '("Diffview"
    ["Align windows" diffview--align-windows]
    ["Next file" diffview--next-file]
    ["Prev file" diffview--prev-file]
    ["Next change" diffview--next-change]
    ["Prev change" diffview--prev-change]
    ["Quit" diffview--quit]))

;;; You probably don't want to invoke `diffview-mode' directly.  Just use
;;; one of the autoload functions above.

(define-derived-mode diffview-mode special-mode "Diffview"
  "Mode for viewing diffs side-by-side"
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(diff-font-lock-keywords t nil nil nil (font-lock-multiline . nil)))
  (use-local-map diffview--mode-map))

(defun diffview--quit ()
  "Quit diffview and clean up diffview buffers."
  (interactive)
  (delete-other-windows)
  (scroll-all-mode 0)
  (let ((plusbuf (get-buffer diffview--plus-bufname))
	(minusbuf (get-buffer diffview--minus-bufname)))
    (if plusbuf (kill-buffer plusbuf))
    (if minusbuf (kill-buffer minusbuf)))
  (set-window-configuration diffview--saved-wincfg))

(defun diffview--next-file (&optional arg)
  "Move to next diff file start. Move to previous diff file start
with prefix ARG."
  (interactive "P")
  (let* ((updown (if arg -1 1))
         (next-file-line-num (save-excursion
                               (save-restriction
                                 (widen)
                                 (let ((old-start-re "^--- ")
                                       (new-start-re "^\\+\\+\\+ "))
                                   (beginning-of-line)
                                   (when (looking-at (if (= updown 1) old-start-re new-start-re))
                                     (forward-line updown))
                                   (when (looking-at (if (= updown 1) new-start-re old-start-re))
                                     (forward-line updown))
                                   (while (and (not (if (= updown 1) (eobp) (bobp)))
                                               (not (looking-at new-start-re)))
                                     (forward-line updown))
                                   (line-number-at-pos))))))
    (let ((n-lines (- next-file-line-num (line-number-at-pos))))
      (when
          (and (not (= n-lines 0))
               (cond
                ((string= (buffer-name (current-buffer))
                          diffview--minus-bufname)
                 (forward-line n-lines)
                 (switch-to-buffer-other-window diffview--plus-bufname))
                ((string= (buffer-name (current-buffer))
                          diffview--plus-bufname)
                 (forward-line n-lines)
                 (switch-to-buffer-other-window diffview--minus-bufname)))
               (forward-line n-lines)
               (other-window 1))))))

(defun diffview--prev-file ()
  "Move to prev diff file start"
  (interactive)
  (diffview--next-file t))

(defun diffview--changed-lines ()
  "Return a vector marking which lines of the side-by-side view differ.

Element N is non-NIL when line N+1 is part of a change on either
side.  Both side-by-side buffers are padded to the same length, so
one vector describes them both."
  (let ((minusbuf (get-buffer diffview--minus-bufname))
        (plusbuf (get-buffer diffview--plus-bufname)))
    (unless (and minusbuf plusbuf)
      (user-error "No side-by-side diff to navigate"))
    (let ((key (list minusbuf plusbuf
                     (buffer-chars-modified-tick minusbuf)
                     (buffer-chars-modified-tick plusbuf))))
      (unless (and diffview--changed-lines
                   (equal key diffview--changed-lines-key))
        (let* ((minus (with-current-buffer minusbuf
                        (split-string (buffer-string) "\n")))
               (plus (with-current-buffer plusbuf
                       (split-string (buffer-string) "\n")))
               (vec (make-vector (max (length minus) (length plus)) nil))
               (i 0))
          (while (or minus plus)
            (let ((m (car minus))
                  (p (car plus)))
              (aset vec i (or (and m (string-match-p diffview--regexp-is-minus-line m))
                              (and p (string-match-p diffview--regexp-is-plus-line p)))))
            (setq minus (cdr minus)
                  plus (cdr plus)
                  i (1+ i)))
          (setq diffview--changed-lines vec
                diffview--changed-lines-key key)))
      diffview--changed-lines)))

(defun diffview--change-start-p (line changed)
  "Return non-NIL if LINE starts a change block according to CHANGED."
  (and (>= line 1)
       (<= line (length changed))
       (aref changed (1- line))
       (or (= line 1)
           (not (aref changed (- line 2))))))

(defun diffview--search-change-start (line step changed)
  "Return the first change block start reached from LINE moving by STEP.
Returns NIL when there is none."
  (let ((l (+ line step))
        (found nil))
    (while (and (not found) (>= l 1) (<= l (length changed)))
      (if (diffview--change-start-p l changed)
          (setq found l)
        (setq l (+ l step))))
    found))

(defun diffview--goto-line-in-both-buffers (line)
  "Move point to LINE in both side-by-side buffers, keeping them aligned.
Point is left in the window it started in."
  (let ((from-top (- (line-number-at-pos (point))
                     (line-number-at-pos (window-start)))))
    (goto-char (point-min))
    (forward-line (1- line))
    (when (cond
           ((string= (buffer-name (current-buffer))
                     diffview--minus-bufname)
            (switch-to-buffer-other-window diffview--plus-bufname))
           ((string= (buffer-name (current-buffer))
                     diffview--plus-bufname)
            (switch-to-buffer-other-window diffview--minus-bufname)))
      (goto-char (point-min))
      (forward-line (1- line))
      (recenter from-top)
      (other-window 1))
    (recenter from-top)))

(defun diffview--move-change (count)
  "Move COUNT change blocks forward, or backward when COUNT is negative."
  (let* ((changed (diffview--changed-lines))
         (step (if (< count 0) -1 1))
         (remaining (abs count))
         (line (line-number-at-pos))
         (target nil))
    (while (> remaining 0)
      (let ((candidate (diffview--search-change-start line step changed)))
        (if candidate
            (setq target candidate
                  line candidate
                  remaining (1- remaining))
          (setq remaining 0))))
    (if target
        (diffview--goto-line-in-both-buffers target)
      (message "No %s change" (if (< step 0) "previous" "next")))))

(defun diffview--next-change (&optional arg)
  "Move to the start of the next change.  With numeric ARG, move ARG changes."
  (interactive "p")
  (diffview--move-change (or arg 1)))

(defun diffview--prev-change (&optional arg)
  "Move to the start of the previous change.  With numeric ARG, move ARG changes."
  (interactive "p")
  (diffview--move-change (- (or arg 1))))

(defun diffview--align-windows ()
  (interactive)
  (let ((align-to-line (line-number-at-pos))
        (align-from-top (- (line-number-at-pos (point))
                           (line-number-at-pos (window-start)))))
    (when
        (cond
         ((string= (buffer-name (current-buffer))
                   diffview--minus-bufname)
          (switch-to-buffer-other-window diffview--plus-bufname))
         ((string= (buffer-name (current-buffer))
                   diffview--plus-bufname)
          (switch-to-buffer-other-window diffview--minus-bufname)))
      (goto-char (point-min))
      (forward-line (1- align-to-line))
      (recenter align-from-top)
      (other-window 1))))

(provide 'diffview)
;;; diffview.el ends here
;;
