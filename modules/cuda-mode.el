;; -*- lexical-binding: t; -*-
(require 'cc-mode)

(provide 'cuda-mode)

;; ;;;###autoload
;; (add-to-list 'auto-mode-alist '("\\.cu$" . cuda-mode))

;; define several category of keywords
(setq cuda-types '("char1" "uchar1" "short1" "ushort1" "int1" "uint1" "long1" "ulong1" "float1"
                   "char2" "uchar2" "short2" "ushort2" "int2" "uint2" "long2" "ulong2" "float2"
                   "char3" "uchar3" "short3" "ushort3" "int3" "uint3" "long3" "ulong3" "float3"
                   "char4" "uchar4" "short4" "ushort4" "int4" "uint4" "long4" "ulong4" "float4"
                   "longlong1" "ulonglong1" "double1"
                   "longlong2" "ulonglong2" "double2"
                   "dim3"))

;; including CUBLAS constants 
(setq cuda-const '("CUBLAS_OP_N" "CUBLAS_OP_T" "CUBLAS_OP_C"
                   "CUBLAS_FILL_MODE_LOWER" "CUBLAS_FILL_MODE_UPPER"
                   "CUBLAS_SIDE_LEFT" "CUBLAS_SIDE_RIGHT"
                   "CUBLAS_POINTER_MODE_HOST" "CUBLAS_POINTER_MODE_DEVICE"))

(setq cuda-keywords '("__device__" "__global__" "__shared__" "__host__" "__constant__" "__noinline__" "__forceinline__"
                      "__restrict__" "__launch_bounds__"))

(setq cuda-builtins '(;; thread management
                      "__syncthreads" "__threadfence_block" "__threadfence" "__threadfence_system"
                      ;; pre-defined variable
                      "gridDim" "blockDim" "blockIdx" "threadIdx" "warpSize"))

;; (setq cuda-func '("\\<\\(\\sw+\\) ?(" "\\<\\(\\sw+\\)<<<"))
(setq cuda-func nil)

;; generate regex string for each category of keywords
(setq cuda-types-regexp (regexp-opt cuda-types 'words))
(setq cuda-const-regexp (regexp-opt cuda-const 'words))
(setq cuda-keywords-regexp (regexp-opt cuda-keywords 'words))
(setq cuda-builtins-regexp (regexp-opt cuda-builtins 'words))
(setq cuda-func-regexp (regexp-opt cuda-func 'words))

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq cuda-font-lock-keywords
      `((,cuda-types-regexp . font-lock-type-face)
        (,cuda-const-regexp . font-lock-constant-face)
        (,cuda-keywords-regexp . font-lock-keyword-face)
        (,cuda-builtins-regexp . font-lock-builtin-face)
        (,cuda-func-regexp . font-lock-function-name-face)
        ("\\<\\(\\sw+\\)[ \t]*\\(<\\{3\\}\\)" (1 font-lock-function-name-face))
        ))

(define-derived-mode cuda-mode c++-mode "CUDA"
  "Major mode for editing CUDA files"
  ;; code for syntax highlighting
  (font-lock-add-keywords nil cuda-font-lock-keywords))

;; clear everything
(setq cuda-types nil)
(setq cuda-const nil)
(setq cuda-keywords nil)
(setq cuda-builtins nil)
(setq cuda-functions nil)

(setq cuda-types-regexp nil)
(setq cuda-const-regexp nil)
(setq cuda-keywords-regexp nil)
(setq cuda-builtins-regexp nil)
(setq cuda-func-regexp nil)
;;; cuda.el ends here
