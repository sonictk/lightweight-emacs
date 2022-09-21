;;; verse-mode.el --- major mode for Verse -*- lexical-binding: t; -*-
(require 'compile) ; for `compilation-error-regexp-alist
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
    "interface"
    "module" 
    "option"
    "struct" 
    "subtype"
    "trait"
    "typedef"
    ; mutability
    "at"
    "set"
    "var"
    ; flow control
    "and"
    "branch"
    "break"
    "case"
    "continue"
    "defer"
    "do"
    "else"
    "for"
    "if"
    "in"
    "loop" 
    "not"
    "of"
    "or"
    "race"
    "return"
    "rush"
    "spawn"
    "sync"
    "then"
    "until"
    "upon"
    "where"
    "while"
    "yield"
    ; attributes
    "abstract"
    "async"
    "computes"
    "converges"
    "decides"
    "epic_internal"
    "final"
    "impure"
    "internal"
    "native"
    "native_callable"
    "no_rollback"
    "override"
    "private"
    "protected"
    "public"
    "pure"
    "reads"
    "suspends"
    "transacts"
    "varies"
    "writes"
    ; identifiers
    "self"
    "self_class"
    "super"
    ; declarations
    "import"
    "using"
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
    "operator"
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
     ; ("!\\|?\\|^\\|:\\|:=\\|;\\|@\\|=\\|<\\|>\\|+\\|-\\|*\\|/" . font-lock-keyword-face)
     ; For character literals
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
     ("<#>\n\\([ \t].+\n\\)+\\|<#\\([^#]*\\)#>" 0 font-lock-comment-face t)
     ("'[[:alnum:]]*'" . font-lock-string-face)
     ; function name/params list/colon/return type
     ; (\w+)(\([\s\w,:;]*\))(:)(\w+) is the original regex with 4 groups using JavaScript syntax
     ; TODO if a failable invocation calls a normal function, the normal function regex overrides the other and vice versa. i.e. `foo[bar()]`
     ("\\([_[:alnum:]]+\\)\\([\(\[][[:alnum:]:, \t?=.]*[\]\)]\\)" (1 font-lock-function-name-face) )
     ("\\([_[:alnum:]]+\\)\\(\([[:alnum:]:, \t?=.]*\)\\)\\(.*\\)\\(:\\)\\([_[:alnum:]]+\\)" (5 font-lock-type-face) )
     ))
)

; TODO: Support all Verse comment styles correctly.
(defvar verse-mode-syntax-table
  (let ((st (make-syntax-table)))
    ; Syntax for curly braces, parentheses and brackets.
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) "))" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    ; Syntax for carets. (That are used in attributes.) 
    ; We can't really use these because it's part of math operators as well.
    (modify-syntax-entry ?< "(<" st)
    (modify-syntax-entry ?> ")>" st)

    ;; - and _ are word constituents
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "w" st)

    ;; String/char delimiters
    ;; ' is a char delimiter; however because we can't put comments in the syntax table we can't put this in either.
    ;; It also interferes with normal grammar usage such as "jane's, peter's", etc.
    ; (modify-syntax-entry ?\' "\"" st)
    ;; " is a string delimiter too
    (modify-syntax-entry ?\" "\"" st)

    ; NOTE Supporting all 3 comment styles of Verse in the syntax table alone...is difficult. Because they share the same character, but in different ways.
    ; So we do it as a regex instead that captures all 3 types. Cause otherwise <# #> and <#> interfere with each other.
    ; Block comment of style <# ... #>
    ; (modify-syntax-entry ?< ". 1" st)
    ; (modify-syntax-entry ?# ". 23" st) 
    ; (modify-syntax-entry ?> ". 4" st)

    ; Line comments of style # ...
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)

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
    ; We can't re-use this as punctuation since it interferes with the syntax table entry for comments.
    ; (modify-syntax-entry ?< ".")
    ; (modify-syntax-entry ?> ".")

    ; Other reserved symbols in Verse as punctuation
    (modify-syntax-entry ?@ ".")
    (modify-syntax-entry ?: ".")
    (modify-syntax-entry ?\; ".")

    ; pointers and optionals as punctuation; this will be superceded by use of `var`/`set`
    (modify-syntax-entry ?? ".")
    (modify-syntax-entry ?^ ".")

    ; whitespace syntax class
    (modify-syntax-entry ?\s " ")
    (modify-syntax-entry ?\t " ")
    st
  )
)

; TODO
; Imenu support 
(defvar verse-imenu-generic-expressions
  `(())
)

; TODO
; (defun verse-indent-line-calculate-indent ()
;   (+ verse-tab-width (current-column)))

(defun verse-indent-line ()
  "Indent current line."
  ; (save-excursion
  ;   (beginning-of-line)
  ;   (indent-line-to (verse-indent-line-calculate-indent))))
  (insert-tab))

(defun verse-indent-region ()
  "Indent current region."
  (insert-tab))


; (defvar verse-mode-abbrev-table nil
;   "Abbreviation table used in `verse-mode' buffers.")
; (define-abbrev-table 'verse-mode-abbrev-table
;   '(
;     ("define" "stub{}:stub{} = stub{}")
;     ("stub" "stub{}")))

;;;###autoload
(define-derived-mode verse-mode fundamental-mode "Verse"
  "VERSE mode is a major mode for editing VERSE files"
  :syntax-table verse-mode-syntax-table
  ;:abbrev-table verse-mode-abbrev-table

  ; Because of how Verse comments work, we cannot have all the rules set in a single 
  ; syntax table together; thus we need to use a regex, and thus we need multiline font-lock
  ; support.
  (setq-local font-lock-multiline t)

  (setq-local font-lock-defaults verse-font-lock-defaults)
  (when verse-tab-width
    (setq-local tab-width verse-tab-width))

  ; Idiomatic Verse code uses spaces instead of tabs, including for indentation.
  (setq-local indent-tabs-mode nil)
  (setq-local tab-always-indent t)
  (setq-local indent-line-function #'verse-indent-line)
  ; (setq-local indent-region-function #'verse-indent-region)
  (setq-local electric-indent-inhibit t)

  (setq-local comment-start "#")
  (setq-local comment-end "")

  ;; Note that there's no need to manually call `verse-mode-hook'; `define-derived-mode'
  ;; will define `verse-mode' to call it properly right before it exits
  ; (font-lock-fontify-buffer)
  ; (abbrev-mode)

  (add-verse-error-regexp)
)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.verse" . verse-mode))
(add-to-list 'auto-mode-alist '("\\.versetest" . verse-mode))

(provide 'verse-mode)
