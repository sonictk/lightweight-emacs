;;; verse-mode.el --- major mode for Verse -*- lexical-binding: t; -*-
(defun add-verse-error-regexp ()
  (setq-default compilation-error-regexp-alist
   (append
    '(
      ; ("\\(D:.+\\.\\(verse\\|versetest\\)\\)\(\\([0-9]+\\),\\([0-9]+\\), \\([0-9]+\\),\\([0-9]+\\)\)" 1 3 4 2)
      )
    compilation-error-regexp-alist
    )
  )
)

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
    "in"
    "loop" 
    "continue"
    "until"
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
    "upon"
    "yield"
    "of"
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
    "transacts"
    "pure"
    "impure"
    "override"
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

(defvar verse-tab-width 4 "Width of a tab for VERSE mode")

; Should look at Simple Minded Indentation Engine and Verse BNF grammar to indent instead
; Otherwise indentation is just wrong

(defvar verse-font-lock-defaults
  `((
     ; Highlight special Verse characters TODO but this breaks other syntax highlighting. Maybe better to define these in the syntax table. Somehow.
     ; ! | ? | ^ | : | := | ; | @ | = | < | > | + | - | * | /
     ("!\\|?\\|^\\|:\\|:=\\|;\\|@\\|=\\|<\\|>\\|+\\|-\\|*\\|/" . font-lock-keyword-face)
     ; For character literals
     ("'[[:alnum:]]+'" . font-lock-string-face)
     ( ,(regexp-opt verse-keywords 'words) . font-lock-keyword-face)
     ( ,(regexp-opt verse-constants 'words) . font-lock-constant-face)
     ( ,(regexp-opt verse-builtin-types 'words) . font-lock-type-face)
     ( ,(regexp-opt verse-builtin-procs 'words) . font-lock-builtin-face)
     ; Single-line comment syntax highlighting
     ; ("#.+" . font-lock-comment-face)
     ; Actual regex for indcmt
     ; <#>\n([ \t].+\n)+
     ; Block comment regex TODO: This won't work well in all cases. Should look at: http://xahlee.info/emacs/emacs_manual/elisp/Multiline-Font-Lock.html#Multiline-Font-Lock
     ; <#([^#]*)#>
     ("<#>\n\\([ \t].+\n\\)+\\|#.+\\|<#\\([^#]*\\)#>" 0 font-lock-comment-face t)
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
    ;; ' is a char delimiter; however because we can't put comments in the syntax table we can't put this in either.
    ; (modify-syntax-entry ?\' "\"" st)
    ;; " is a string delimiter too
    (modify-syntax-entry ?\" "\"" st)

    ; NOTE Supporting all 3 comment styles of Verse in the syntax table alone...is difficult.
    ; So we do it as a regex instead that captures all 3 types. Cause otherwise <# #> and <#> interfere with each other.
    ; Block comment of style <# ... #>
    ; (modify-syntax-entry ?< ". 1" st)
    ; (modify-syntax-entry ?# ". 23" st) 
    ; (modify-syntax-entry ?> ". 4" st)

    ; Line comments of style # ...
    ; (modify-syntax-entry ?# "<" st)
    ; (modify-syntax-entry ?\n ">" st)

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

  (setq font-lock-defaults verse-font-lock-defaults)
  (when verse-tab-width
    (setq tab-width verse-tab-width))

  ;(setq indent-line-function #'verse-indent-line)
  ; Idiomatic Verse code uses spaces instead of tabs, including for indentation.
  (setq indent-tabs-mode nil)

  (setq comment-start "#")
  (setq comment-end "")

  ;; Note that there's no need to manually call `verse-mode-hook'; `define-derived-mode'
  ;; will define `verse-mode' to call it properly right before it exits
  ; (font-lock-fontify-buffer)
  ;(abbrev-mode)

  (add-verse-error-regexp)
)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.verse" . verse-mode))
(add-to-list 'auto-mode-alist '("\\.versetest" . verse-mode))

(provide 'verse-mode)
