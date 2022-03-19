;;; verse-mode.el --- major mode for Verse -*- lexical-binding: t; -*-
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
  '("true"
    "false"))

(defvar verse-keywords
  '( ; data classes
    "class" 
    "enum"
    "module" 
    "optional"
    "typedef"
    "subtype"
    "interface"
    "trait"
    "enum"
    ; flow control
    "if"
    "then"
    "else"
    "for"
    "loop" 
    "break"
    "return"
    "case"
    "where"
    "do"
    "defer"
    "while"
    "branch"
    "sync"
    "race"
    "rush"
    "spawn"
    ; attributes
    "public"
    "internal"
    "protected"
    "private"
    "native"
    "native_callable"
    "async"
    "abstract"
    "final"
    "decides"
    ; identifiers
    "self"
    "self_class"
    "super"
    ; declarations
    "using"
    "import"
    ))

(defvar verse-builtin-procs
  '(
    "assert"
    "Floor"
    "Abs"
    "Length"
    "Concat"
    "Contains"
    "type"
   )
)

(defvar verse-builtin-types
  '(
    "int"
    "string"
    "char"
    "float"
    "rat"
    "logic"
    "array"
    "map"
    "tuple"
    "void"
    )
)

;; I'd probably put in a default that you want, as opposed to nil
(defvar verse-tab-width 4 "Width of a tab for VERSE mode")

; TODO this only works for curly braces
; Should look at Simple Minded Indentation Engine and Verse BNF grammar to indent instead
(defun verse-indent-line ()
  "Indent current line."
  (let (indent
        boi-p                           ;begin of indent
        move-eol-p
        (point (point)))                ;lisps-2 are truly wonderful
    (save-excursion
      (back-to-indentation)
      (setq indent (car (syntax-ppss))
            boi-p (= point (point)))
      ;; don't indent empty lines if they don't have the in it
      (when (and (eq (char-after) ?\n)
                 (not boi-p))
        (setq indent 0))
      ;; check whether we want to move to the end of line
      (when boi-p
        (setq move-eol-p t))
      ;; decrement the indent if the first character on the line is a
      ;; closer.
      (when (or (eq (char-after) ?\))
                (eq (char-after) ?\}))
        (setq indent (1- indent)))
      ;; indent the line
      (delete-region (line-beginning-position)
                     (point))
      (indent-to (* tab-width indent)))
    (when move-eol-p
      (move-end-of-line nil))))

;; Two small edits.
;; First is to put an extra set of parens () around the list
;; which is the format that font-lock-defaults wants
;; Second, you used ' (quote) at the outermost level where you wanted ` (backquote)
;; you were very close
(defvar verse-font-lock-defaults
  `((
     ;; stuff between double quotes
     ("\"\\.\\*\\?" . font-lock-string-face)
     ; ("<@[^>]*>" . font-lock-constant-face)
     ;; ; : , ; { } =>  @ $ = are all special elements
     ; (":\\|,\\|;\\|{\\|}\\|=>\\|@\\|$\\|=" . font-lock-keyword-face)
     ( ,(regexp-opt verse-keywords 'words) . font-lock-keyword-face)
     ( ,(regexp-opt verse-constants 'words) . font-lock-constant-face)
     ( ,(regexp-opt verse-builtin-types 'words) . font-lock-type-face)
     ( ,(regexp-opt verse-builtin-procs 'words) . font-lock-builtin-face)
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

  (setq indent-line-function #'verse-indent-line)
  ; Idiomatic Verse code uses spaces instead of tabs, including for indentation.
  (setq indent-tabs-mode nil)

  ;; for comments
  ;; overriding these vars gets you what (I think) you want
  ;; they're made buffer local when you set them
  (setq comment-start "#")
  (setq comment-end "")

  ;; add comments. lua-mode does something similar, so it shouldn't
  ;; bee *too* wrong.
  (modify-syntax-entry ?# "<" verse-mode-syntax-table)
  (modify-syntax-entry ?\n ">" verse-mode-syntax-table)

  (modify-syntax-entry ?\{ "(}" verse-mode-syntax-table)
  (modify-syntax-entry ?\} "){" verse-mode-syntax-table)
  (modify-syntax-entry ?\( "()" verse-mode-syntax-table)

  ;; - and _ are word constituents
  (modify-syntax-entry ?_ "w" verse-mode-syntax-table)
  (modify-syntax-entry ?- "w" verse-mode-syntax-table)

  ;; both single and double quotes makes strings
  (modify-syntax-entry ?\" "\"" verse-mode-syntax-table)
  (modify-syntax-entry ?' "'" verse-mode-syntax-table)

  ;; '==' as punctuation
  (modify-syntax-entry ?= ".")

  ;; Note that there's no need to manually call `verse-mode-hook'; `define-derived-mode'
  ;; will define `verse-mode' to call it properly right before it exits
  )

(provide 'verse-mode)
