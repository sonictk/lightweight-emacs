; Add custom module path so that nothing is saved to the global emacs config
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/")
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/yasnippet")
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/swift-mode")
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/ivy")

; Determine the underlying operating system
(setq lightweight-aquamacs (string-equal system-type "darwin"))
(setq lightweight-linux (featurep 'x))
(setq lightweight-win32 (not (or lightweight-aquamacs lightweight-linux)))

; Blink the cursor forever
(setq blink-cursor-blinks -1)

; Increase line number limit for really large files
(setq line-number-display-limit-width 9999999)

; Set the default directory for find-file
(setq default-directory "~/")

; Template system for Emacs - allows macros to do text insertion
(require 'yasnippet)
(yas-global-mode 1)

; On OSX, this is required in order to have Emacs have access to the same binaries 
; i.e. /usr/loca/bin that the shell normally would have. Yay Apple!
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-copy-env "PYTHONPATH")
  (exec-path-from-shell-initialize))

(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key (kbd "C-c h h") 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
; Define more readable faces for highlighting
(set-face-attribute 'highlight-symbol-face nil
                    :weight 'bold
                    :background "gray10"
                    :foreground "hot pink"
                    :underline t)
(defface highlight-symbol-face2
  '((t :foreground "SpringGreen3"
       :background "gray10"
       :weight bold
       :underline t
       ))
  "Face used by `highlight-symbol-mode'."
  :group 'highlight-symbol)
(defface highlight-symbol-face3
  '((t :foreground "yellow1"
       :background "gray10"
       :weight bold
       :underline t
       ))
  "Face used by `highlight-symbol-mode'."
  :group 'highlight-symbol)
(defface highlight-symbol-face4
  '((t :foreground "cyan"
       :background "gray10"
       :weight bold
       :underline t
       ))
  "Face used by `highlight-symbol-mode'."
  :group 'highlight-symbol)
(defface highlight-symbol-face5
  '((t :foreground "violet"
       :background "gray10"
       :weight bold
       :underline t
       ))
  "Face used by `highlight-symbol-mode'."
  :group 'highlight-symbol)
(defface highlight-symbol-face6
  '((t :foreground "firebrick1"
       :background "gray10"
       :weight bold
       :underline t
       ))
  "Face used by `highlight-symbol-mode'."
  :group 'highlight-symbol)
(setq highlight-symbol-colors '("highlight-symbol-face" 
                                "highlight-symbol-face2"
                                "highlight-symbol-face3"
                                "highlight-symbol-face4"
                                "highlight-symbol-face5"
                                "highlight-symbol-face6"))

; Allow for side-by-side diff viewing
(require 'diffview)

; Binding for line wrapping
(global-set-key (kbd "C-M-S-w") 'visual-line-mode)

(setq imenu-max-item-length 255)

(add-to-list 'completion-styles 'substring)

(require 'ivy)
(require 'counsel)
(require 'swiper)
(ivy-mode 1)
(counsel-mode 1)

;; better performance on everything (especially windows), ivy-0.10.0 required
;; @see https://github.com/abo-abo/swiper/issues/1218
(setq ivy-dynamic-exhibit-delay-ms 250)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))
;; enable this if you want `swiper' to use it
(setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c h i") 'counsel-imenu)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

; This is to allow for reverse isearch to be integrated with swiper
(defun swiper-C-r (&optional arg)
  "Move cursor vertically down ARG candidates.
If the input is empty, select the previous history element instead."
  (interactive "p")
  (if (string= ivy-text "")
      (ivy-next-history-element 1)
    (ivy-previous-line arg)))
 (define-key ivy-minibuffer-map (kbd "C-r") 'swiper-C-r)

; Makes C-s C-w work like normal isearch when using ivy
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)

(require 'ivy-xref)
;; xref initialization is different in Emacs 27 - there are two different
;; variables which can be set rather than just one
(when (>= emacs-major-version 27)
  (setq xref-show-definitions-function #'ivy-xref-show-defs))
;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
;; commands other than xref-find-definitions (e.g. project-find-regexp)
;; as well
(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode objc-mode cuda-mode) "clangd"))
(add-to-list 'eglot-server-programs
             `(python-mode . ("pyls" "-v" "--tcp" "--host"
                              "localhost" "--port" :autoport)))

(when lightweight-aquamacs
(add-to-list 'eglot-server-programs '(swift-mode . ("/Applications/XcodeBeta.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")))
(add-to-list 'eglot-server-programs '(csharp-mode . ("/usr/local/bin/omnisharp" "-lsp"))))

(when lightweight-linux
(add-to-list 'eglot-server-programs '(csharp-mode . ("/usr/local/bin/omnisharp" "-lsp"))))

(when lightweight-win32
(add-to-list 'eglot-server-programs '(csharp-mode . ("C:/omnisharp/OmniSharp.exe" "-lsp"))))

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'objc-mode-hook 'eglot-ensure)
(add-hook 'csharp-mode-hook 'eglot-ensure)
(add-hook 'swift-mode 'eglot-ensure)
; Python already works OOTB

; Don't want the eldoc box showing everywhere, have a global bind for it
(add-hook 'eglot--managed-mode-hook #'eldoc-box-hover-mode t)
(setq global-eldoc-mode nil)
(setq eldoc-idle-delay 1.0)
(add-to-list 'eglot-ignored-server-capabilites :hoverProvider)

(require 'cc-mode)
;(define-key c-mode-base-map (kbd "M-RET") 'eglot-rename)
(global-set-key (kbd "M-RET") 'eglot-rename)
(global-set-key (kbd "M-,") 'xref-find-definitions-other-window)
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key [C-mouse-1] 'xref-find-defintions-at-mouse)
(global-set-key [C-mouse-2] 'eldoc-box-eglot-help-at-point)
(global-set-key (kbd "C-c ?") 'eldoc-box-eglot-help-at-point)

(require 'bison-mode)

; Allow for manual-rescanning of buffers
 (defun rescan-symbols-in-buffer()
   (interactive)
   (imenu--menubar-select imenu--rescan-item))

; Do not save semanticdb file to user home emacs directory
(setq semanticdb-default-save-directory "~/Git/lightweight-emacs/semantic-cache")

; Configure scrolling to only scroll half a page at a time
(require 'view)
(global-set-key "\C-v"   'View-scroll-half-page-forward)
(global-set-key "\M-v"   'View-scroll-half-page-backward)

;; insert an empty line after the current line and position the cursor on its beginning
(defun insert-empty-line ()
 (interactive)
 (move-end-of-line nil)
 (open-line 1)
 (next-line 1))
(defun insert-empty-line-backwards ()
 (interactive)
 (move-beginning-of-line nil)
 (open-line 1))

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")
;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key [(shift return)] 'open-next-line)
(global-set-key [(ctrl return)] 'open-next-line)
(global-set-key [(ctrl shift return)] 'insert-empty-line-backwards)

; Allow for loading recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(setq recentf-save-file (expand-file-name "recentf" "~/Git/lightweight-emacs/"))

; Allow toggling hiding of comments
(require 'hide-comnt)

; Highlight doxygen comments
(require 'doxymacs)
(defun my-doxymacs-font-lock-hook ()
    (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
        (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
; Auto reload-buffers when files are changed on disk
(global-auto-revert-mode t)

; Enable Emacs Development Environment mode
(global-ede-mode t)

; Enable remote .dir-locals.el files to be found
(setq enable-remote-dir-locals t)

; Define function to reload all directory local vars for all buffers
(defun reload-dir-locals-for-current-buffer ()
  "Reload directory-local variables for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))
(defun reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (reload-dir-locals-for-current-buffer)))))

(global-set-key (kbd "C-M-S-l") 'reload-dir-locals-for-all-buffer-in-this-directory)

; Set swapping between header/implementation files to work
(setq-default ff-other-file-alist
  '(("\\.cpp\\'" (".hpp" ".ipp" ".h"))
    ("\\.ipp\\'" (".hpp" ".cpp"))
    ("\\.hpp\\'" (".ipp" ".cpp" ".cxx"))
    ("\\.cxx\\'" (".hxx" ".ixx" ".h"))
    ("\\.ixx\\'" (".cxx" ".hxx" ".h"))
    ("\\.hxx\\'" (".ixx" ".cxx" ".cpp"))
    ("\\.c\\'" (".h"))
    ("\\.m\\'" (".h"))
    ("\\.mm\\'" (".h"))
    ("\\.metal\\'" (".h")) 
    ("\\.h\\'" (".c" ".cpp" ".C" ".CC" ".cxx" ".ixx" ".ipp" ".m" ".mm")))
)

; Clang-format functionality
(require 'clang-format)
(global-set-key (kbd "C-M-S-c") 'clang-format-region)

; Function to kill all other buffers apart from the current one
(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
        (delq (current-buffer)
            (remove-if-not 'buffer-file-name (buffer-list))
        )
    )
)

; Disable auto-saving buffers
(setq auto-save-default nil)
(setq auto-save-interval 0)
(setq auto-save-timeout 0)

; Disable word wrapping by default
(set-default 'truncate-lines t)

; Undo functionality improved
(require 'undo-tree)
(global-undo-tree-mode)

; Allow for swapping buffers between windows
(require 'buffer-move)
(global-set-key (kbd "<M-S-left>") 'buf-move-left)
(global-set-key (kbd "<M-S-right>") 'buf-move-right)

; Stop Emacs from losing undo information by
; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(global-hl-line-mode 1)

; For long lines, Emacs slows down with hl-line-mode.
(require 'so-long)
(global-so-long-mode 1)

; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-screen t)

(setq compilation-directory-locked nil)
(scroll-bar-mode -1)
(setq shift-select-mode nil)
(setq enable-local-variables nil)
(setq lightweight-font "outline-DejaVu Sans Mono")

; Only display the line numbers when goto line is activated
(global-set-key [remap goto-line] 'goto-line-with-feedback)

; Allow for going to specific line number
(defun goto-line-with-feedback ()   "Show line numbers temporarily, while prompting for the line number input"   (interactive)   (unwind-protect
      (progn
        (display-line-numbers-mode 1)
        (goto-line (read-number "Goto line: ")))
    (display-line-numbers-mode -1)))

; Allow for displaying column no. in mode line
(setq column-number-mode t)

(when lightweight-win32
  (setq lightweight-font "outline-Liberation Mono")
)

; ; (when lightweight-aquamacs
; ;   (cua-mode 0)
; ;   (tabbar-mode 0)
; ;   (setq mac-command-modifier 'meta)
; ;   (setq x-select-enable-clipboard t)
; ;   (setq aquamacs-save-options-on-quit 0)
; ;   (setq special-display-regexps nil)
; ;   (setq special-display-buffer-names nil)
; ;   (define-key function-key-map [return] [13])
; ;   (setq mac-command-key-is-meta t)
; ;   (scroll-bar-mode nil)
; ;   (setq mac-pass-command-to-system nil)
; ; )

(when lightweight-linux
  (display-battery-mode 1)
)

; Turn off the toolbar
(tool-bar-mode 0)

(load-library "view")
(require 'compile)

; Set the default compilation command to use CMake
(setq compile-command "cmake --build . --config Debug --target INSTALL")

(defun lightweight-ediff-setup-windows (buffer-A buffer-B buffer-C control-buffer)
  (ediff-setup-windows-plain buffer-A buffer-B buffer-C control-buffer)
)
(setq ediff-window-setup-function 'lightweight-ediff-setup-windows)
(setq ediff-split-window-function 'split-window-horizontally)

; Restore session after ediff session
(defvar my-ediff-last-windows nil)

(defun my-store-pre-ediff-winconfig ()
  (setq my-ediff-last-windows (current-window-configuration)))

(defun my-restore-pre-ediff-winconfig ()
  (set-window-configuration my-ediff-last-windows))

(add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig)

; Turn off the bell on Mac OS X
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)

; Setup my compilation mode
(defun lightweight-compilation-hook ()
  (make-local-variable 'truncate-lines)
  (setq truncate-lines nil)
)

(add-hook 'compilation-mode-hook 'lightweight-compilation-hook)

(defun insert-timeofday ()
   (interactive "*")
   (insert (format-time-string "%a, %d %b %y: %I:%M%p")))

; Add support for cmake-files
(require 'cmake-mode)

; Add support for CUDA kernel files
(require 'cuda-mode)

; Add support for Apple Swift language 
(require 'swift-mode)

; Highlight escape character sequences correctly
(custom-set-variables
 '(hes-mode-alist
   (quote
    ((c-mode . "\\(\\\\\\([0-7]\\{1,3\\}\\|x[[:xdigit:]]+\\|u[[:xdigit:]]\\{4\\}\\|U[[:xdigit:]]\\{8\\}\\|[\"'?\\abfnrtv]\\)\\)")
     (c++-mode . "\\(\\\\\\([0-7]\\{1,3\\}\\|x[[:xdigit:]]+\\|u[[:xdigit:]]\\{4\\}\\|U[[:xdigit:]]\\{8\\}\\|[\"'?\\abfnrtv]\\)\\)")
     (cmake-mode . "\\(\\\\\\([0-7]\\{1,3\\}\\|x[[:xdigit:]]+\\|u[[:xdigit:]]\\{4\\}\\|U[[:xdigit:]]\\{8\\}\\|[\"'?\\abfnrtv]\\)\\)")
     (objc-mode . "\\(\\\\\\([0-7]\\{1,3\\}\\|x[[:xdigit:]]+\\|u[[:xdigit:]]\\{4\\}\\|U[[:xdigit:]]\\{8\\}\\|[\"'?\\abfnrtv]\\)\\)")
     (python-mode . "\\(\\\\\\([0-7]\\{1,3\\}\\|x[[:xdigit:]]+\\|u[[:xdigit:]]\\{4\\}\\|U[[:xdigit:]]\\{8\\}\\|[\"'?\\abfnrtv]\\)\\)")
     (java-mode . "\\(\\\\\\([0-7]\\{1,3\\}\\|u[[:xdigit:]]\\{4\\}\\|[\"'\\bfnrt]\\)\\)")
     (clojure-mode . "\\(\\\\\\([0-7]\\{1,3\\}\\|u[[:xdigit:]]\\{4\\}\\|[\"'\\bfnrt]\\)\\)")
     (js-mode . "\\(\\\\\\([0-7]\\{1,3\\}\\|x[[:xdigit:]]\\{2\\}\\|u[[:xdigit:]]\\{4\\}\\|.\\)\\)")
     (js2-mode . "\\(\\\\\\([0-7]\\{1,3\\}\\|x[[:xdigit:]]\\{2\\}\\|u[[:xdigit:]]\\{4\\}\\|.\\)\\)")
     (ruby-mode
      ("\\(\\\\\\([0-7]\\{1,3\\}\\|x[[:xdigit:]]\\{1,2\\}\\|u\\(?:[[:xdigit:]]\\{4\\}\\|{[[:xdigit:]]\\{1,6\\}\\(?:[[:space:]]+[[:xdigit:]]\\{1,6\\}\\)*}\\)\\|.\\)\\)"
       (0
        (let*
            ((state
              (syntax-ppss))
             (term
              (nth 3 state)))
          (when
              (or
               (and
                (eq term 39)
                (member
                 (match-string 2)
                 (quote
                  ("\\" "'"))))
               (if
                   (fboundp
                    (quote ruby-syntax-expansion-allowed-p))
                   (ruby-syntax-expansion-allowed-p state)
                 (memq term
                       (quote
                        (34 47 10 96 t)))))
            (font-lock-prepend-text-property
             (match-beginning 1)
             (match-end 1)
             (quote face)
             (quote hes-escape-backslash-face))
            (font-lock-prepend-text-property
             (match-beginning 2)
             (match-end 2)
             (quote face)
             (quote hes-escape-sequence-face))
            nil))
        prepend)))
     (emacs-lisp-mode . "\\(\\\\\\(u[[:xdigit:]]\\{4\\}\\|U00[[:xdigit:]]\\{6\\}\\|x[[:xdigit:]]+\\|[0-7]+\\|.\\)\\)"))))
 '(yascroll:delay-to-hide nil)
 '(cursor-color "#40FF40")
)

; Accepted file extensions and their appropriate modes
(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
         ("\\.hin$"    . c++-mode)
         ("\\.cin$"    . c++-mode)
         ("\\.inl$"    . c++-mode)
         ("\\.rdc$"    . c++-mode)
         ("\\.h$"    . c++-mode)
         ("\\.c$"   . c++-mode)
         ("\\.cc$"   . c++-mode)
         ("\\.c8$"   . c++-mode)
         ("\\.metal$"   . c++-mode) ; TODO: Should probably fix this when a real Metal package appears on the scene
         ("\\.txt$" . indented-text-mode)
         ("\\.emacs$" . emacs-lisp-mode)
         ("\\.gen$" . gen-mode)
         ("\\.ms$" . fundamental-mode)
         ("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)
         ("\\.m$" . objc-mode)
         ("\\.mm$" . objc-mode)
         ("\\.cu$" . cuda-mode)
         ("\\.swift$" . swift-mode)
         ) auto-mode-alist))

; C++ indentation style
(setq c-default-style "k&r"
      c-basic-offset 4)

; Set tab width to 4 by default and use spaces by default
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent t)
(setq-default c-tab-always-indent nil)
(electric-indent-mode 1)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                          64 68 72 76 80 84 88 92 96 100 104 108 112
                          116 120 124 128 132 136 140 144 148 152 156
                          160))

; Set flag so that will not be prompted to kill running process on closing Emacs every single time
(add-hook 'comint-exec-hook
      (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

; Add syntax highlighting for escape characters
(require 'highlight-escape-sequences)
(hes-mode t)

; Mouse keybinding for rectangle mark mode
(defun mouse-start-rectangle (start-event)
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point start-event)
  (rectangle-mark-mode +1)
  (let ((drag-event))
    (track-mouse
      (while (progn
               (setq drag-event (read-event))
               (mouse-movement-p drag-event))
        (mouse-set-point drag-event)))))

(global-set-key (kbd "S-<down-mouse-3>") 'mouse-start-rectangle)

; Fix scroll-all-mode not working with the mouse wheel
(defun mwheel-scroll-all-function-all (func &optional arg)
  (if (and scroll-all-mode arg)
      (save-selected-window
        (walk-windows
         (lambda (win)
           (select-window win)
           (condition-case nil
               (funcall func arg)
             (error nil)))))
    (funcall func arg)))

(defun mwheel-scroll-all-scroll-up-all (&optional arg)
  (mwheel-scroll-all-function-all 'scroll-up arg))

(defun mwheel-scroll-all-scroll-down-all (&optional arg)
  (mwheel-scroll-all-function-all 'scroll-down arg))

(setq mwheel-scroll-up-function 'mwheel-scroll-all-scroll-up-all)
(setq mwheel-scroll-down-function 'mwheel-scroll-all-scroll-down-all)

; Transparency toggle configuration
(defvar emacs-transparency-toggle-switch nil)

(defun emacs-transparency-toggle ()
  (interactive)
  (if emacs-transparency-toggle-switch
      (progn
        (setq emacs-transparency-toggle-switch nil)
        (set-frame-parameter nil 'alpha 100))
    (setq emacs-transparency-toggle-switch t)
(set-frame-parameter nil 'alpha 50)))

; Set CMake tab width to use 4 spaces instead of 2 by defaul
(setq cmake-tab-width 4)

; Set GUD-GDB hotkeys
(global-set-key (kbd "<C-S-f8>") 'gud-nexti)
(global-set-key (kbd "<f8>") 'gud-next)
(global-set-key (kbd "<M-f5>") 'gud-finish)
(global-set-key (kbd "<f5>") 'gud-go)
(global-set-key (kbd "<C-f8>") 'gud-break)
(global-set-key (kbd "<M-f8>") 'gud-remove)
(global-set-key (kbd "<C-f5>") 'gud-until)
(global-set-key (kbd "<C-S-f7>") 'gud-stepi)
(global-set-key (kbd "<f7>") 'gud-step)

; GDB control which windows new buffers are displayed in.
;(add-to-list 'display-buffer-alist
;         (cons 'cdb-source-code-buffer-p
;           (cons 'display-source-code-buffer nil)))
;
;(defun cdb-source-code-buffer-p (bufName action)
;  "Return whether BUFNAME is a source code buffer."
;  (let ((buf (get-buffer bufName)))
;    (and buf
;     (with-current-buffer buf
;       (derived-mode-p buf 'c++-mode 'c-mode 'csharp-mode 'nxml-mode)))))
;
;(defun display-source-code-buffer (sourceBuf alist)
;  "Find a window with source code and set sourceBuf inside it."
;  (let* ((curbuf (current-buffer))
;     (wincurbuf (get-buffer-window curbuf))
;     (win (if (and wincurbuf
;               (derived-mode-p sourceBuf 'c++-mode 'c-mode 'nxml-mode)
;               (derived-mode-p (current-buffer) 'c++-mode 'c-mode 'nxml-mode))
;          wincurbuf
;        (get-window-with-predicate
;         (lambda (window)
;           (let ((bufName (buffer-name (window-buffer window))))
;             (or (cdb-source-code-buffer-p bufName nil)
;             (assoc bufName display-buffer-alist)
;             ))))))) ;; derived-mode-p doesn't work inside this, don't know why...
;    (set-window-buffer win sourceBuf)
;    win))
;

; Force the interpreter window to scroll when typing something in gdb, 
; and scroll the window when there is output and when the window is not in focus.
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output 'others)

; GDB Restore windows layout after debugging and also nicer default layout
(setq gdb-many-windows nil)
(defun set-gdb-layout(&optional c-buffer)
  (if (not c-buffer)
      (setq c-buffer (window-buffer (selected-window)))) ;; save current buffer

  ;; from http://stackoverflow.com/q/39762833/846686
  (set-window-dedicated-p (selected-window) nil) ;; unset dedicate state if needed
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows) ;; clean all

  (let* (
         (w-source (selected-window)) ;; left top
         (w-gdb (split-window w-source nil 'right)) ;; right bottom
         (w-locals (split-window w-gdb nil 'above)) ;; right middle bottom
         (w-stack (split-window w-locals nil 'above)) ;; right middle top
         (w-breakpoints (split-window w-stack nil 'above)) ;; right top
         (w-io (split-window w-source (floor(* 0.9 (window-body-height)))
                             'below)) ;; left bottom
         )
    (set-window-buffer w-io (gdb-get-buffer-create 'gdb-inferior-io))
    (set-window-dedicated-p w-io t)
    (set-window-buffer w-breakpoints (gdb-get-buffer-create 'gdb-breakpoints-buffer))
    (set-window-dedicated-p w-breakpoints t)
    (set-window-buffer w-locals (gdb-get-buffer-create 'gdb-locals-buffer))
    (set-window-dedicated-p w-locals t)
    (set-window-buffer w-stack (gdb-get-buffer-create 'gdb-stack-buffer))
    (set-window-dedicated-p w-stack t)
    (set-window-dedicated-p w-gdb t)

    (set-window-buffer w-gdb gud-comint-buffer)

    (select-window w-source)
    (set-window-buffer w-source c-buffer)
    ))
(defadvice gdb (around args activate)
  "Change the way to gdb works."
  (setq global-config-editing (current-window-configuration)) ;; to restore: (set-window-configuration c-editing)
  (let (
        (c-buffer (window-buffer (selected-window))) ;; save current buffer
        )
    ad-do-it
    (set-gdb-layout c-buffer))
  )
(defadvice gdb-reset (around args activate)
  "Change the way to gdb exit."
  ad-do-it
  (set-window-configuration global-config-editing))

; Support LLDB debugger
(require 'gud-lldb)

; Function for displaying the file name in the minibuffer
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

; Function for clearing the kill ring
(defun clear-kill-ring ()
  "Clears the kill ring of all entries."
  (interactive) (setq kill-ring nil))

; Function for getting current reference to file name and line position.
(defun copy-current-line-position-to-clipboard ()
"Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
(interactive)
(let ((path-with-line-number
       (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
  (kill-new path-with-line-number)
  (message (concat path-with-line-number " copied to clipboard"))))

; Add python-mode syntax hook for SCons files
(setq auto-mode-alist
     (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
     (cons '("SConscript" . python-mode) auto-mode-alist))

(require 'eldoc-box)
(setq x-gtk-resize-child-frames 'resize-mode)
(setq eldoc-box-max-pixel-width 1900)
(setq eldoc-box-max-pixel-height 1000)

; CC++ mode handling
(defun lightweight-c-hook ()
  ; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode t)

  ; Additional style stuff
  (c-set-offset 'member-init-intro '++)
  (c-set-offset 'inline-open '0)

  ; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  ; Newline indents, semi-colon doesn't
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))

  ; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)

  ; Abbrevation expansion
  (abbrev-mode 1)

  ; devenv.com error parsing
  (add-to-list 'compilation-error-regexp-alist 'lightweight-devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(lightweight-devenv
   "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
    2 3 nil (4)))

  ; Unbind c bug report submission
  (define-key c++-mode-map (kbd "C-c C-b") nil)
  (define-key c-mode-map (kbd "C-c C-b") nil)
)

(defun lightweight-replace-string (FromString ToString)
  "Replace a string without moving point."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    (replace-string FromString ToString)
  ))

(add-hook 'c-mode-common-hook 'lightweight-c-hook)

; Allow for code folding
(add-hook 'c-mode-common-hook 'hs-minor-mode)

(defun lightweight-save-buffer ()
  "Save the buffer after untabifying it."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (untabify (point-min) (point-max))))
  (save-buffer))

; TXT mode handling
(defun lightweight-text-hook ()
  ; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode nil)
)

(add-hook 'text-mode-hook 'lightweight-text-hook)

; Window Commands
(defun w32-restore-frame ()
    "Restore a minimized frame"
     (interactive)
     (w32-send-sys-command 61728))

; Navigation
(defun previous-blank-line ()
  "Moves to the previous line containing nothing but whitespace."
  (interactive)
  (search-backward-regexp "^[ \t]*\n")
)

(defun next-blank-line ()
  "Moves to the next line containing nothing but whitespace."
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1)
)

(define-key global-map [C-right] 'forward-word)
(define-key global-map [C-left] 'backward-word)
(define-key global-map [C-up] 'previous-blank-line)
(define-key global-map [C-down] 'next-blank-line)
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)
(define-key global-map [pgup] 'forward-page)
(define-key global-map [pgdown] 'backward-page)
(define-key global-map [C-next] 'scroll-other-window)
(define-key global-map [C-prior] 'scroll-other-window-down)

(defun append-as-kill ()
  "Performs copy-region-as-kill as an append."
  (interactive)
  (append-next-kill)
  (copy-region-as-kill (mark) (point))
)
(define-key global-map [M-up] 'previous-blank-line)
(define-key global-map [M-down] 'next-blank-line)
(define-key global-map [M-right] 'forward-word)
(define-key global-map [M-left] 'backward-word)

(define-key global-map [f9] 'first-error)
(define-key global-map [f10] 'previous-error)
(define-key global-map [f11] 'next-error)

(define-key global-map "\en" 'next-error)
(define-key global-map "\eN" 'previous-error)

; Remap the command and alt keys on OSX
; Also allows for fn-delete to be right-delete
(if (or (eq system-type 'darwin) (eq system-type 'gnu/linux))
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier nil)
    (global-set-key [kp-delete] 'delete-char)
)

; Enable subword mode globally by default
(global-subword-mode t)
(global-set-key (kbd "C-c C-w") 'subword-mode)

; Set key bindings for kill word and backward kill word that are overridden by Prelude
(global-set-key (kbd "C-<backspace>") 'backward-kill-word)
    (global-set-key (kbd "C-<delete>") 'kill-word)

; Set key bindings for in-place scrolling of window
(global-set-key (kbd "M-n") 'scroll-up-line)
(global-set-key (kbd "M-p") 'scroll-down-line)

; Unbind MMB
(global-unset-key (kbd "<mouse-2>"))

; Bind additional yank command hotkey
(global-set-key (kbd "C-S-v") 'yank)

;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
(setq save-interprogram-paste-before-kill t)

; Better yank ring management
(require 'browse-kill-ring)
(global-set-key (kbd "M-y") 'browse-kill-ring)
(setq browse-kill-ring-highlight-current-entry t)

; Unbind suspending the frame and bind it to undo instead
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

; Bindings for smooth horizontal scrolling
(global-unset-key (kbd "C-x >"))
(global-unset-key (kbd "C-x <"))
(global-set-key (kbd "C-x >") '(lambda ()(interactive)(scroll-left 15)))
(global-set-key (kbd "C-x <") '(lambda ()(interactive)(scroll-right 15)))

; Bindings for commenting
(defun comment-or-uncomment-region-or-line()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))
(global-unset-key (kbd "C-c C-c"))
(global-unset-key (kbd "C-/"))
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

; Bindings for opening recent files
(global-set-key (kbd "C-x C-S-f") 'recentf-open-files)

; Bindings for mousewheel horizontal scrolling
(global-set-key (kbd "<S-wheel-down>") '(lambda nil (interactive) (scroll-right 15)))
(global-set-key (kbd "<S-double-wheel-down>") '(lambda nil (interactive) (scroll-right 15)))
(global-set-key (kbd "<S-triple-wheel-down>") '(lambda nil (interactive) (scroll-right 15)))
(global-set-key (kbd "<S-mouse-4>") '(lambda nil (interactive) (scroll-right 15)))
(global-set-key (kbd "<wheel-right>") '(lambda nil (interactive) (scroll-right 15)))
(global-set-key (kbd "<wheel-left>") '(lambda nil (interactive) (scroll-left 15)))
(global-set-key (kbd "<S-wheel-up>") '(lambda nil (interactive) (scroll-left 15)))
(global-set-key (kbd "<S-double-wheel-up>") '(lambda nil (interactive) (scroll-left 15)))
(global-set-key (kbd "<S-triple-wheel-up>") '(lambda nil (interactive) (scroll-left 15)))
(global-set-key (kbd "<S-mouse-5>") '(lambda nil (interactive) (scroll-left 15)))

; Bindings for text scale adjustment via scrollwheel
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-double-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-triple-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-double-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-triple-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

; Set ECB mode to use LMB instead of MMB for selection
(setq ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))

(global-set-key (kbd "<mouse-4>") '(lambda nil (interactive) (scroll-down 6)))
(global-set-key (kbd "<mouse-5>") '(lambda nil (interactive) (scroll-up 6)))

; Additional keybinds for moving lines up/down on the home row
(require 'move-text)
(global-set-key (kbd "C-S-p") 'move-text-up)
(global-set-key (kbd "C-S-n") 'move-text-down)

; Project management using Projectile
(require 'projectile)
(projectile-global-mode t)
; Force Projectile to use faster indexing in Windows
; NOTE: If this causes problems, comment it out
(setq projectile-indexing-method 'alien)
; Remove redundant project name from the mode line
; (setq projectile-mode-line '(:eval (format "[%s]" (projectile-project-name)))) 
(setq projectile-mode-line '(:eval (format "" )))

(setq projectile-completion-system 'ivy)

; As of latest projectile 1.1.0, ``projectile-keymap-prefix`` is deprecated and need 
; to use this instead to set keybindings.
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

; Additional keybindngs for finding header files
(global-set-key (kbd "C-,") 'projectile-find-other-file)
(global-set-key (kbd "C-.") 'projectile-find-other-file-other-window)

; Additional keybinding for finding symbol globally within project
(global-set-key (kbd "C-M-S-s") 'projectile-find-tag)

; Search using the silver searcher
(global-set-key (kbd "C-S-f") 'projectile-ag)

; Editing
(defun lightweight-replace-in-region (old-word new-word)
  "Perform a replace-string in the current region."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion (save-restriction
		    (narrow-to-region (mark) (point))
		    (beginning-of-buffer)
		    (replace-string old-word new-word)
		    ))
  )
(define-key global-map (kbd "C-%") 'lightweight-replace-in-region)

; Compilation
(setq compilation-context-lines 0)
(setq compilation-error-regexp-alist
    (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
     compilation-error-regexp-alist))

(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

; Commands
(set-variable 'grep-command "grep -irHn ")
(when lightweight-win32
    (set-variable 'grep-command "findstr -s -n -i -l "))

; Smooth scroll
(setq scroll-step 3)

; Set display margins
(defun my-set-margins ()
  "Set margins in current buffer."
  (setq left-margin-width 4)
  (setq right-margin-width 4)
)
(add-hook 'text-mode-hook 'my-set-margins)

; Markdown support
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md.html\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; scroll 3 lines at a time when using mwheel
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

; Scroll just one line when hitting bottom of window
(setq scroll-conservatively 10000)

; Startup windowing
(setq next-line-add-newlines nil)
(setq truncate-partial-width-windows nil)

; Global scrollbar mode
(require 'yascroll)
(global-yascroll-bar-mode 1)

; Disabled for now since a lot of programmers don't understand that trailing whitespace is stupid
; and this causes lots of diffs.
; Use whitespace cleaning only for programming modes
; (add-hook 'prog-mode-hook
;     (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(auto-save-list-file-prefix nil)
 '(auto-save-timeout 0)
 '(auto-show-mode t t)
 '(delete-auto-save-files nil)
 '(delete-old-versions (quote other))
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 500000)
 '(kept-new-versions 5)
 '(kept-old-versions 5)
 '(make-backup-file-name-function (quote ignore))
 '(make-backup-files nil)
 '(mouse-wheel-follow-mouse nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (15)))
 '(version-control nil))

(defun lightweight-never-split-a-window nil)
(setq split-window-preferred-function 'lightweight-never-split-a-window)

; Check if running on Macbook based off hostname and set the font size accordingly
(if (string-equal system-name "sonictk-mbp.local")
    (progn 
        (add-to-list 'default-frame-alist '(font . "Liberation Mono-14"))
        (set-face-attribute 'default nil :font "Liberation Mono-14"))
    (progn
        (add-to-list 'default-frame-alist '(font . "Liberation Mono-11.5"))
        (set-face-attribute 'default t :font "Liberation Mono-11.5"))
)

(if (string-equal system-name "sonictk-mbp-aapl.local")
    (progn 
        (add-to-list 'default-frame-alist '(font . "Liberation Mono-16"))
        (set-face-attribute 'default nil :font "Liberation Mono-16"))
    (progn
        (add-to-list 'default-frame-alist '(font . "Liberation Mono-13"))
        (set-face-attribute 'default t :font "Liberation Mono-13"))
)

(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/Git/lightweight-emacs/themes"))

; Highlight TODOs and other interesting code tags along with whitespace/tabs
(defface note-face
  '((t :foreground "SpringGreen3"
       :slant italic
       :weight bold
       :underline t
       ))
  "Face for note comments."
  :group 'basic-faces)
(defface todo-face
  '((t :foreground "tomato1"
       :slant italic
       :weight bold
       :underline t
       ))
  "Face for todo comments."
  :group 'basic-faces)
(defvar note-font-lock-face 'note-face) ; This is needed for font-lock to access the face
(defvar todo-font-lock-face 'todo-face) ; This is needed for font-lock to access the face
(custom-set-faces
  '(font-lock-warning-face ((t (:foreground "pink" :underline t :slant italic :weight bold))))
  '(hes-escape-backslash-face ((t (:foreground "tan" :slant italic :weight bold))))
  '(hes-escape-sequence-face ((t (:foreground "tan" :slant italic :weight bold))))
  '(hi-blue-b ((t (:foreground "sandy brown" :weight bold))))

  '(linum-face ((t (:foreground "peru" :background "#3F3F3F"))))

  '(whitespace-space ((t (:bold t :foreground "gray37" :background "gray24"))))
  '(whitespace-empty ((t (:foreground "gray37" :background "gray24"))))
  '(whitespace-hspace ((t (:foreground "gray37" :background "gray24"))))
  '(whitespace-indentation ((t (:foreground "gray37" :background "gray24"))))
  '(whitespace-line ((t (:foreground "gray37" :background "gray24"))))
  '(whitespace-newline ((t (:foreground "gray37" :background "gray24"))))
  '(whitespace-space-after-tab ((t (:foreground "gray37" :background "gray24"))))
  '(whitespace-space-before-tab ((t (:foreground "gray37" :background "gray24"))))
  '(whitespace-tab ((t (:foreground "gray37" :background "gray24"))))
  '(whitespace-trailing ((t (:foreground "gray37" :background "gray24"))))
)
(defun font-lock-comment-annotations ()
    "Highlight a bunch of well known comment annotations.
  This functions should be added to the hooks of major modes for programming."
    (font-lock-add-keywords
         nil '(("\\<\\(FIX\\(ME\\)?\\|fixme\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\|todo\\|optimize\\|hack\\|refactor\\)"
                          1 todo-font-lock-face t)))
    (font-lock-add-keywords
         nil '(("\\<\\(NOTE\\|note\\)" 1 note-font-lock-face t)))
)
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

; Add C++11 keywords highlighting 
(font-lock-add-keywords 'c++-mode
                        '(("\\bconstexpr\\b" . 'font-lock-keyword-face) 
                          ("\\boverride\\b" . 'font-lock-keyword-face) 
                          ("\\bfinal\\b" . 'font-lock-keyword-face)))

; Add rainbow delimiters highlighting
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

; Use whitespace cleaning only for programming modes
(add-hook 'prog-mode-hook
    (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(require 'whitespace)
; Show whitespace
(global-whitespace-mode t)
(setq show-trailing-whitespace t)
(setq whitespace-style (quote
   (face spaces tabs space-mark tab-mark)))
(setq whitespace-display-mappings
  ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
  '(
    (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    ;(newline-mark 10 [182 10]) ; 10 LINE FEED
    (tab-mark 9 [187 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
   )
)

; Set up auto-complete for code
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/company-mode/")
(require 'company)
(setq company-backends (delete 'company-semantic company-backends))
;(define-key c-mode-map  [(ctrl tab)] 'company-complete)
;(define-key c++-mode-map  [(ctrl tab)] 'company-complete)
;(define-key python-mode-map  [(ctrl tab)] 'company-complete)
(global-set-key [(ctrl tab)] 'company-complete)

; Disable idle completion, idle is the devil's work
(setq company-idle-delay nil)

; Increase completion time for larger C++ projects
(setq company-async-timeout 10)

; Company GUI settings
(setq company-show-numbers t)
(setq company-tooltip-maximum-width 100)
(setq company-tooltip-limit 15)
(setq company-selection-wrap-around t)


; FIX for fci-mode distorting the popup for company completions
(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
; FIX ends here

;; Windows performance tweaks
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))

; Live syntax checking
(require 'let-alist)
(require 'pkg-info)
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/flycheck")
(require 'flycheck)
(global-flycheck-mode -1) ; Disable globally by default

; Set cc-search-directories as safe in order to allow ff-find-other-file to work
(require 'find-file)
(put 'cc-search-directories 'safe-local-variable #'listp)
(put 'cc-other-file-alist 'safe-local-variable #'listp)
(put 'flycheck-clang-include-path 'safe-local-variable #'listp)

; Don't clutter the modeline
(setq eldoc-minor-mode-string nil)

; Startup with split window
(split-window-horizontally)

; Shortcut to yes/no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

; Guess indentation in files
(require 'dtrt-indent)
(dtrt-indent-mode 1)
(setq dtrt-indent-verbosity 0)

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

; Set up auto-pairing for parentheses
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/smartparens")
(require 'dash)
(require 'smartparens-config)
(add-to-list 'sp-ignore-modes-list '(minibuffer-inactive-mode picture-mode markdown-mode text-mode artist-mode))
(show-smartparens-global-mode t)
(smartparens-global-mode nil)

(add-hook 'artist-mode-hook
    (lambda ()
      (turn-off-smartparens-mode)
      (whitespace-mode)
    )
)

(add-hook 'prog-mode-hook #'smartparens-mode)

(setq sp-show-pair-delay 0.8) ; Slow down the smartparens matching mode to improve interactive typing performance

; when you press RET, the curly braces automatically
; add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))

; Show possible commands in minibuffer after hitting first button combination
(require 'which-key)
(which-key-mode)

; Copy/cut line if no region is selected
(defun my-kill-ring-save (beg end flash)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end) nil)
                 (list (line-beginning-position)
                       (line-beginning-position 2) 'flash)))
  (kill-ring-save beg end)
  (when flash
    (save-excursion
      (if (equal (current-column) 0)
          (goto-char end)
        (goto-char beg))
      (sit-for blink-matching-delay))))
(global-set-key [remap kill-ring-save] 'my-kill-ring-save)

(put 'kill-region 'interactive-form      
 '(interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

; Function for finding all subdirectories for setting in cc-search-directories
(defun get-all-subdirectories(dir-list)
  "Returns a list of all recursive subdirectories of dir-list,
   ignoring directories with names that start with . (dot)"
  (split-string
   (shell-command-to-string
     (concat "find "
             (mapconcat 'identity dir-list " ")
             " -type d -not -regex \".*/\\\..*\""))))

; Make Emacs not throw warnings on UTF-8 encoded Python scripts
(define-coding-system-alias 'UTF-8 'utf-8) 

; Default M-x pdb is incorrect
(setq gud-pdb-command-name "python -m pdb")

; Enable generating Sphinx-compatible docstrings automatically for Python with C-c C-d
(add-hook 'python-mode-hook (
        lambda ()
        (require 'sphinx-doc)
        (sphinx-doc-mode t)
    )
)

; Use js-mode to highlight JSON files
(autoload 'js-mode "js-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

; Add MEL mode syntax highlighting
(autoload 'mel-mode "mel-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.mel$" . mel-mode))

; Add MaxScript mode syntax highlighting
(autoload 'maxscript-mode "maxscript-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ms$" . maxscript-mode))
(add-hook 'maxscript-mode
  (lambda()(dtrt-indent-mode t))
  (lambda()(indent-tabs-mode t))
)

; Add support for C#
(autoload 'csharp-mode "csharp-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

; Add support for the Rust programming language
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

; Add support for NASM syntax highlighting
(autoload 'nasm-mode "nasm-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))
(add-to-list 'auto-mode-alist '("\\.\\(inc\\|s\\)$" . nasm-mode))

; Shader syntax highlighting
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

(autoload 'hlsl-mode "hlsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.fx\\'" . hlsl-mode))
(add-to-list 'auto-mode-alist '("\\.hlsl\\'" . hlsl-mode))

; Coffeescript support for syntax highlighting
(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))

; Better eshell
(setq eshell-history-size 1024)
(setq eshell-prompt-regexp "^[^#$]*[#$] ")

(load "em-hist")           ; So the history vars are defined
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t)) ; Don't ask, just save
;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)
(if (boundp 'eshell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always)) ; For older(?) version
;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)
(defun eshell/ef (fname-regexp &rest dir) (ef fname-regexp default-directory))

;;; ---- path manipulation
(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
   (home-len (length home)))
    (if (and
   (>= (length pwd) home-len)
   (equal home (substring pwd 0 home-len)))
  (concat "~" (substring pwd home-len))
      pwd)))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat "["
              (if (> (length git-output) 0)
                  (substring git-output 0 -1)
                "(no branch)")
              "]") 'face `(:foreground "green"))
      )))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize ((lambda (p-lst)
            (if (> (length p-lst) 3)
                (concat
                 (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                            (substring elm 0 1)))
                            (butlast p-lst 3)
                            "/")
                 "/"
                 (mapconcat (lambda (elm) elm)
                            (last p-lst 3)
                            "/"))
              (mapconcat (lambda (elm) elm)
                         p-lst
                         "/")))
          (split-string (pwd-repl-home (eshell/pwd)) "/")) 'face `(:foreground "yellow"))
         (or (curr-dir-git-branch-string (eshell/pwd)))
         (propertize "# " 'face 'default))))
(setq eshell-highlight-prompt nil)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

; Save the Emacs sessions
(setq desktop-dirname             "~/Git/lightweight-emacs/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-restore-eager       5 ; Number of buffers to restore immediately; rest are lazily loaded when emacs is idle
      desktop-lazy-verbose        nil
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil
      desktop-auto-save-timeout   30)
(desktop-save-mode 1)
(setq desktop-minor-mode-table '((auto-fill-function auto-fill-mode)
                                 (defining-kbd-macro nil)
                                 (isearch-mode nil)
                                 (vc-mode nil)
                                 (vc-dired-mode nil)
                                 (erc-track-minor-mode nil)
                                 (savehist-mode nil)
                                 (global-whitespace-mode nil)
                                 (global-smartparens-mode nil))) ; Prevent desktop read from being slow

; Allow for switching between visible windows with Shift + arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

; Back button functionality and buffer mark navigation improved
(require 'back-button)
; Override the default keybindings for buffer mark navigation
; Account for both kinds of mice and OSes at work/home
(global-set-key (kbd "<mouse-8>") 'back-button-global-backward)
(global-set-key (kbd "<mouse-9>") 'back-button-global-forward)
(global-set-key (kbd "M-<mouse-8>") 'back-button-local-forward)
(global-set-key (kbd "M-<mouse-9>") 'back-button-local-backward)

(global-set-key (kbd "C-c <C-right>") 'back-button-global-forward)
(global-set-key (kbd "C-c <C-left>") 'back-button-global-backward)
(global-set-key (kbd "C-c C-f") 'back-button-global-forward)
(global-set-key (kbd "C-c C-b") 'back-button-global-backward)

(global-set-key (kbd "C-c <right>") 'back-button-local-forward)
(global-set-key (kbd "C-c <left>") 'back-button-local-backward)
(global-set-key (kbd "C-c b") 'back-button-local-forward)
(global-set-key (kbd "C-c f") 'back-button-local-backward)
(back-button-mode 1)

; Disable back button mode showing in the modeline
(setq back-button-mode-lighter nil)

; Cycle between cases
(require 'string-inflection)
(global-set-key (kbd "C-c u") 'string-inflection-all-cycle)
;; for ruby
(add-hook 'ruby-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c u") 'string-inflection-ruby-style-cycle)))
;; for java
(add-hook 'java-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c u") 'string-inflection-java-style-cycle)))

; Colours in eshell
(require 'xterm-color)
(require 'eshell)
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq xterm-color-preserve-properties t)))
(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
(setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

; Allow for running compile in a specific directory
(require 'cd-compile)
(global-set-key (kbd "C-S-b") 'cd-compile)

; Use the silver searcher instead of grep for searching
(require 'ag)

; Scroll output in the compilation window automatically
(setq compilation-scroll-output t)

; Make GC not happen too much for unicode buffers 
(setq inhibit-compacting-font-caches t)

; Use speedbar in same frame
(require 'sr-speedbar)
(setq speedbar-use-images nil)
(global-set-key (kbd "C-S-s") 'sr-speedbar-toggle)

; Use smaller speedbar font
(make-face 'speedbar-face)
(set-face-font 'speedbar-face "Liberation Mono-10")
(setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))
(setq sr-speedbar-skip-other-window-p t) ; Supposed to fix issues when calling speedbar before sr-speedbar

(defun select-next-window ()
  (other-window 1))
(defun my-sr-speedbar-open-hook ()
  (add-hook 'speedbar-before-visiting-file-hook 'select-next-window t)
  (add-hook 'speedbar-before-visiting-tag-hook 'select-next-window t)
  )
(advice-add 'sr-speedbar-open :after #'my-sr-speedbar-open-hook)

; Allow for completions when using interactive search
(eval-after-load "isearch"
  '(progn
     (require 'isearch-dabbrev)
     (define-key isearch-mode-map (kbd "M-/") 'isearch-dabbrev-expand))
)

; Disable garbage collection while the minibuffer is open
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

; Aliases for unintuitive commands
(defalias 'refresh-syntax-highlighting 'font-lock-fontify-buffer)

; Cleanup and theme setup
(defun post-load-stuff ()
  (interactive)
  (menu-bar-mode -1)
  (load-theme 'zenburn t)
  (set-cursor-color "#40FF40")
  (set-face-background 'hl-line "#1a3a3a")
  (if (eq system-type 'windows-nt)
    (w32-send-sys-command 61488) ; Maximize window
  )
  (recentf-load-list)
  (global-company-mode t)
  (setq fill-column 81)
)
(add-hook 'window-setup-hook 'post-load-stuff t)
