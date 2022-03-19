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
    "false"
  )
)

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
  )
)

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

; Should look at Simple Minded Indentation Engine and Verse BNF grammar to indent instead
; Otherwise indentation is just wrong

;; Two small edits.
;; First is to put an extra set of parens () around the list
;; which is the format that font-lock-defaults wants
;; Second, you used ' (quote) at the outermost level where you wanted ` (backquote)
;; you were very close
(defvar verse-font-lock-defaults
  `((
     ;; ; : , ; { } =>  @ $ = are all special elements
     ; (":\\|,\\|;\\|{\\|}\\|=>\\|@\\|$\\|=" . font-lock-keyword-face)
     ; Highlight special Verse characters TODO but this breaks other syntax highlighting. Maybe better to define these in the syntax table.
     ; ! | ? | ^ | : | := | ; | @ | = | < | > | + | - | * | /
     ; ("!\\|?\\|^\\|:\\|:=\\|;\\|@\\|=\\|<\\|>\\|+\\|-\\|*\\|/" . font-lock-keyword-face)
     ("#.+" . font-lock-comment-face)
     ( ,(regexp-opt verse-keywords 'words) . font-lock-keyword-face)
     ( ,(regexp-opt verse-constants 'words) . font-lock-constant-face)
     ( ,(regexp-opt verse-builtin-types 'words) . font-lock-type-face)
     ( ,(regexp-opt verse-builtin-procs 'words) . font-lock-builtin-face)
     ))
)

; TODO: Support all Verse comment styles correctly.
(defvar verse-mode-syntax-table
  (let ((st (make-syntax-table)))
    ; Syntax for curly braces and parentheses.
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) "))" st)
    ; Syntax for brackets.
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    ; Syntax for carets. But this interferes with block comments!
    ; (modify-syntax-entry ?< "(>" st)
    ; (modify-syntax-entry ?> ")<" st)

    ;; - and _ are word constituents
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "w" st)

    ;; String/char delimiters
    ;; ' is a char delimiter
    (modify-syntax-entry ?\' "\"" st)
    ;; " is a string delimiter too
    (modify-syntax-entry ?\" "\"" st)

    ; Block comment of style <# ... #>
    (modify-syntax-entry ?< ". 1bn" st)
    (modify-syntax-entry ?# ". 23bn" st) 
    (modify-syntax-entry ?> ". 24bn" st)

    ; Line comments of style # ...
    ;  (modify-syntax-entry ?# "<" st)
    ;  (modify-syntax-entry ?\n ">" st)
    
    ; TODO Can I combine both types of comments in the same syntax table?

    ; TODO How to support <#> indented-style comments? Probably not with the syntax table, will need a regexp.

    ; '==' as punctuation
    (modify-syntax-entry ?= ".")

    ; math operators as punctuation
    (modify-syntax-entry ?+ ".")
    (modify-syntax-entry ?- ".")
    (modify-syntax-entry ?* ".")
    (modify-syntax-entry ?\/ ".")

    ; Other ops as punctuation (!, &&, ||)
    (modify-syntax-entry ?! ".")
    (modify-syntax-entry ?& ".")
    (modify-syntax-entry ?| ".")
    (modify-syntax-entry ?: ".")

    ; pointers and optionals as punctuation
    (modify-syntax-entry ?? ".")
    (modify-syntax-entry ?^ ".")
    st
  )
)

; (defvar verse-mode-abbrev-table nil
;   "Abbreviation table used in `verse-mode' buffers.")
; (define-abbrev-table 'verse-mode-abbrev-table
;   '(
;     ("imp" "import {}")
;     ("use" "using {}")
;     ))

;;;###autoload
(define-derived-mode verse-mode fundamental-mode "Verse"
  "VERSE mode is a major mode for editing VERSE files"
  :syntax-table verse-mode-syntax-table
  ;:abbrev-table verse-mode-abbrev-table

  ;; you again used quote when you had '((verse-hilite))
  ;; I just updated the variable to have the proper nesting (as noted above)
  ;; and use the value directly here
  (setq font-lock-defaults verse-font-lock-defaults)

  ;; when there's an override, use it
  ;; otherwise it gets the default value
  (when verse-tab-width
    (setq tab-width verse-tab-width))

  ;(setq indent-line-function #'verse-indent-line)
  ; Idiomatic Verse code uses spaces instead of tabs, including for indentation.
  (setq indent-tabs-mode nil)

  ;; for comments
  ;; overriding these vars gets you what (I think) you want
  ;; they're made buffer local when you set them
  (setq comment-start "#")
  ; (setq comment-end "")

  ;; Note that there's no need to manually call `verse-mode-hook'; `define-derived-mode'
  ;; will define `verse-mode' to call it properly right before it exits
  ; (font-lock-fontify-buffer)
  ;(abbrev-mode)
)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.verse" . verse-mode))
(add-to-list 'auto-mode-alist '("\\.versetest" . verse-mode))

(provide 'verse-mode)
