(defun add-verse-error-regexp ()
  (setq-default compilation-error-regexp-alist
   (append
    '(
      ("\\(D:.+\\.\\(verse\\|versetest\\)\\)\(\\([0-9]+\\),\\([0-9]+\\), \\([0-9]+\\),\\([0-9]+\\)\)" 1 3 4 2)
      )
    compilation-error-regexp-alist
    )
  )
)

(add-verse-error-regexp)
(defvar verse-constants
  '("reservedword1"
    "reservedword2"))

(defvar verse-keywords
  '("class" "string"))

;; I'd probably put in a default that you want, as opposed to nil
(defvar verse-tab-width 4 "Width of a tab for VERSE mode")

;; Two small edits.
;; First is to put an extra set of parens () around the list
;; which is the format that font-lock-defaults wants
;; Second, you used ' (quote) at the outermost level where you wanted ` (backquote)
;; you were very close
(defvar verse-font-lock-defaults
  `((
     ;; stuff between double quotes
     ("\"\\.\\*\\?" . font-lock-string-face)
     ("<@[^>]*>" . font-lock-constant-face)
     ;; ; : , ; { } =>  @ $ = are all special elements
     (":\\|,\\|;\\|{\\|}\\|=>\\|@\\|$\\|=" . font-lock-keyword-face)
     ( ,(regexp-opt verse-keywords 'words) . font-lock-builtin-face)
     ( ,(regexp-opt verse-constants 'words) . font-lock-constant-face)
     )))

(define-derived-mode verse-mode fundamental-mode "Verse"
  "VERSE mode is a major mode for editing VERSE files"
  ;; you again used quote when you had '((verse-hilite))
  ;; I just updated the variable to have the proper nesting (as noted above)
  ;; and use the value directly here
  (setq font-lock-defaults verse-font-lock-defaults)

  ;; when there's an override, use it
  ;; otherwise it gets the default value
  (when verse-tab-width
    (setq tab-width verse-tab-width))

  ;; for comments
  ;; overriding these vars gets you what (I think) you want
  ;; they're made buffer local when you set them
  (setq comment-start "#")
  (setq comment-end "")

  (modify-syntax-entry ?# "< b" verse-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" verse-mode-syntax-table)

  ;; Note that there's no need to manually call `verse-mode-hook'; `define-derived-mode'
  ;; will define `verse-mode' to call it properly right before it exits
  )

(provide 'verse-mode)
