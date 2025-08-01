;; -*- lexical-binding: t; -*-
; Lightweight Emacs configuration file
(setq use-package-always-ensure nil) ; Never download from ELPA

; Add custom module path so that nothing is saved to the global emacs config
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/")
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/yasnippet")
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/yasnippet-snippets")
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/swift-mode")
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/wgrep")
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/rg")
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/compat.el") ; transient.el requires this dependency
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/consult") 
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/consult-company") 
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/consult-eglot") 
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/consult-eglot/extensions/consult-eglot-embark") 
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/vertico") 
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/vertico/extensions") 
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/embark") 
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/marginalia") 
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/orderless") 
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/Git/lightweight-emacs/themes"))

; Enable font ligatures
; Fira Code configuration below
(require 'ligature)
;; Enable the www ligature in every possible major mode
(ligature-set-ligatures 't '("www"))

;; Enable ligatures in programming modes                                                           
(ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
(add-hook 'ediff-startup-hook
  (lambda ()
    (global-ligature-mode 0)))
(add-hook 'ediff-quit-hook
  (lambda ()
    (global-ligature-mode 't)))
(global-ligature-mode 't)

; Use ibuffer instead of list-buffers for the default hotkey.
(global-set-key (kbd "C-x C-b") 'ibuffer)

; TODO: Figure out what's wrong with the config that makes whitespace mode not work with tree-sitter modes.
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (c "https://github.com/tree-sitter/tree-sitter-c.git")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp.git")
     (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp.git")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq treesit-font-lock-level 4)

; tree-sitter defaults to 2 space indentation for some reason
(setq typescript-ts-mode-indent-offset 4)
(setq c-ts-mode-indent-offset 4)
(setq c-ts-mode-indent-style 'bsd)
; (c-ts-mode-set-global-style 'bsd)
(setq sgml-basic-offset 4)

; Use tree-sitter for C/C++
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist
             '(c-or-c++-mode . c-or-c++-ts-mode))

(add-to-list 'major-mode-remap-alist '(csharp-mode . csharp-ts-mode))

(setq edebug-print-length 9999)
(setq edebug-print-level 9999)

; Project management using `project.el`
(require 'project)

(global-set-key (kbd "C-,") 'ff-find-other-file)
(global-set-key (kbd "C-.") 'ff-find-other-file-other-window)

; Template system for Emacs - allows macros to do text insertion
(require 'yasnippet)
(require 'yasnippet-snippets)
(setq yas-snippet-dirs '("~/Git/lightweight-emacs/modules/yasnippet-snippets/snippets"))

; Set up auto-complete for code
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/company-mode/")
(require 'company)
(setq company-backends (delete 'company-semantic company-backends))
(global-set-key [(ctrl tab)] 'company-complete)
; Alternative keybinding as well.
(global-set-key (kbd "C-S-SPC") 'company-complete)

; Allow raw insertion of tabs when the auto-indentation is wonky.
(global-set-key (kbd "C-S-<tab>") '(lambda () (interactive) (insert-char ?\t)))

; Disable idle completion, idle is the devil's work
(setq company-idle-delay nil)

; Increase completion time for larger C++ projects
(setq company-async-timeout 25)

; Company GUI settings
(setq company-show-numbers t)
(setq company-tooltip-maximum-width 200)
(setq company-tooltip-limit 20)
(setq company-selection-wrap-around t)
(setq company-require-match nil)
(setq company-lighter-base "")
(setq company-tooltip-flip-when-above t)
(setq company-tooltip-align-annotations t)
(setq company-tooltip-margin 2)

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

; Limit number of async compilation processes to prevent errors on Windows.
(defconst dd/using-native-comp (and (fboundp 'native-comp-available-p)
                                    (native-comp-available-p)))
(setq native-comp-deferred-compilation t)
(setq native-comp-async-query-on-exit t)
(setq native-comp-async-jobs-number 4)
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-speed 3)

; Because `view-hello-file` is slow, we unbind the keybind that could accidentally trigger it
(define-key global-map (kbd "C-h h") nil)

; Unbind Emacs news hotkeys
(define-key global-map (kbd "C-h n") nil)
(define-key global-map (kbd "C-h C-n") nil)

(require 'consult)
(require 'consult-compile)
(require 'consult-flymake)
(require 'consult-imenu)
(require 'consult-info)
(require 'consult-kmacro)
(require 'consult-org)
(require 'consult-register)
(require 'consult-xref)
(require 'consult-imenu)
(require 'embark)
(require 'embark-consult)
(require 'marginalia)
(require 'orderless)
(require 'consult-company)

(require 'vertico)
(require 'vertico-sort)
(require 'vertico-directory)
(require 'vertico-mouse)
(require 'vertico-multiform)
(require 'vertico-indexed)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :custom
  (savehist-additional-variables '(command-history))
  :init
  (savehist-mode))

(use-package vertico
  :custom
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  (vertico-sort-function #'vertico-sort-history-length-alpha)
  :ensure nil
  :init
  (vertico-mode))

(use-package vertico-sort
  :after vertico
  :ensure nil)

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("C-v" . vertico-scroll-up)
              ("M-v" . vertico-scroll-down)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-mouse
  :after vertico
  :ensure nil)

(use-package vertico-multiform
  :after vertico
  :ensure nil)

(use-package vertico-indexed
  :after vertico
  :ensure nil)

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic gpartial-completion)))))

(define-key company-mode-map [remap completion-at-point] #'consult-company)
(define-key company-active-map [remap pixel-scroll-interpolate-up] 'company-previous-page)
(define-key company-active-map [remap pixel-scroll-interpolate-down] 'company-next-page)
(define-key company-active-map [remap View-scroll-page-backward] 'company-previous-page)
(define-key company-active-map [remap View-scroll-page-forward] 'company-next-page)
(define-key company-active-map [remap View-scroll-half-page-backward] 'company-previous-page)
(define-key company-active-map [remap View-scroll-half-page-forward] 'company-next-page)
(define-key company-active-map [remap end-of-buffer] 'company-select-last)
(define-key company-active-map [remap beginning-of-buffer] 'company-select-first)

; (add-to-list 'completion-styles 'substring)

; Allow consult-line to work better with the current symbol under the cursor.
(defun consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

; Start consult-ripgrep search with the active region.
(defun wrapper/consult-ripgrep (&optional dir given-initial)
  "Pass the region to consult-ripgrep if available.

DIR and GIVEN-INITIAL match the method signature of `consult-wrapper'."
  (interactive "P")
  (let ((initial
         (or given-initial
             (when (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))))))
    (consult-ripgrep dir initial)))

(defun wrapper/consult-line (&optional given-initial)
  "Pass the region to consult-line if available.

GIVEN-INITIAL match the method signature of `consult-wrapper'."
  (interactive "P")
  (let ((initial
         (or given-initial
             (when (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))))))
    (consult-line initial)))

;; Bindings for Consult
(global-set-key (kbd "C-c M-x") 'consult-mode-command)
(global-set-key (kbd "C-c H") 'consult-history)
(global-set-key (kbd "C-c k") 'consult-kmacro)
(global-set-key (kbd "C-c m") 'consult-man)
(global-set-key (kbd "C-c i") 'consult-info)
(global-set-key [remap Info-search] 'consult-info)

(global-set-key (kbd "C-x M-:") 'consult-complex-command)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window)
(global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)
(global-set-key (kbd "C-x r b") 'consult-bookmark)
(global-set-key (kbd "C-x p b") 'consult-project-buffer)

(global-set-key (kbd "M-#") 'consult-register-load)
(global-set-key (kbd "M-'") 'consult-register-store)
(global-set-key (kbd "C-M-#") 'consult-register)

(global-set-key (kbd "M-y") 'consult-yank-pop)

(global-set-key (kbd "M-g e") 'consult-compile-error)
(global-set-key (kbd "M-g f") 'consult-flymake)
(global-set-key (kbd "M-g g") 'consult-goto-line)
; (global-set-key (kbd "M-g M-g") 'consult-goto-line)
(global-set-key (kbd "M-g o") 'consult-outline)
(global-set-key (kbd "M-g m") 'consult-mark)
(global-set-key (kbd "M-g k") 'consult-global-mark)
(global-set-key (kbd "M-g i") 'consult-treesit-imenu)
(global-set-key (kbd "M-g M-i") 'consult-imenu)
(global-set-key (kbd "M-g I") 'consult-treesit-imenu-multi)
(global-set-key (kbd "M-g M-I") 'consult-imenu-multi)

(global-set-key (kbd "M-s d") 'consult-find)
(global-set-key (kbd "M-s D") 'consult-locate)
; (global-set-key (kbd "M-s g") 'consult-grep)
(global-set-key (kbd "M-s G") 'consult-git-grep)
(global-set-key (kbd "M-s r") 'wrapper/consult-ripgrep)
; (global-set-key (kbd "M-s l") 'consult-line-symbol-at-point)
(global-set-key (kbd "M-s l") 'wrapper/consult-line)
(global-set-key (kbd "M-s L") 'consult-line-multi)
(global-set-key (kbd "M-s k") 'consult-keep-lines)
(global-set-key (kbd "M-s u") 'consult-focus-lines)

; Custom keybinding for consult-fd
(global-set-key (kbd "M-s f") 'consult-fd)

(define-key isearch-mode-map (kbd "M-e") 'consult-isearch-history)
(define-key isearch-mode-map (kbd "M-s e") 'consult-isearch-history)
(define-key isearch-mode-map (kbd "M-s l") 'consult-line)
(define-key isearch-mode-map (kbd "M-s L") 'consult-line-multi)

(define-key minibuffer-local-map (kbd "M-s") 'consult-history)
(define-key minibuffer-local-map (kbd "M-r") 'consult-history)

;; Enable automatic preview at point in the *Completions* buffer. This is
;; relevant when you use the default completion UI.
(add-hook 'completion-list-mode #'consult-preview-at-point-mode)

;; Optionally configure the register formatting. This improves the register
;; preview for `consult-register', `consult-register-load',
;; `consult-register-store' and the Emacs built-ins.
(setq register-preview-delay 0.5
      register-preview-function #'consult-register-format)

; Prevent `consult-line` from slowing down in slightly larger buffers due to inefficient syntax highlighting
(setq consult-fontify-max-size 1024)

;; Optionally tweak the register preview window.
;; This adds thin lines, sorting and hides the mode line of the window.
(advice-add #'register-preview :override #'consult-register-window)

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref 
      xref-show-definitions-function #'consult-xref)

;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)

; fd support in consult
(defvar consult--fd-command nil)
(defun consult--fd-builder (input)
  (unless consult--fd-command
    (setq consult--fd-command
          (if (eq 0 (call-process-shell-command "fdfind"))
              "fdfind"
            "fd")))
  (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler
                                      arg 'extended t)))
    (when re
      (cons (append
             (list consult--fd-command
                   "--color=never" "--full-path"
                   (consult--join-regexps re 'extended))
             opts)
            hl))))

(defun consult-fd (&optional dir initial)
  (interactive "P")
  (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" dir))
               (default-directory dir))
    (find-file (consult--find prompt #'consult--fd-builder initial))))

(marginalia-mode 1)

;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
;; strategy, if you want to see the documentation from multiple providers.
; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)

(add-hook 'embark-collect-mode #'consult-preview-at-point-mode)

;; Hide the mode line of the Embark live/completions buffers
(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

; Haskell support
(add-to-list 'load-path "~/Git/lightweight-emacs/modules/haskell-mode")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/Git/lightweight-emacs/modules/haskell-mode")

; Determine the underlying operating system
(setq lightweight-aquamacs (string-equal system-type "darwin"))
(setq lightweight-linux (featurep 'x))
(setq lightweight-win32 (not (or lightweight-aquamacs lightweight-linux)))

; Blink the cursor forever
(setq blink-cursor-blinks -1)

; Increase line number limit for really large files
; (setq line-number-display-limit-width 9999999)

; Set the default directory for find-file
(setq default-directory "~/")

; Improve performance reading from LSP servers
(setq read-process-output-max 1048576)

; On OSX, this is required in order to have Emacs have access to the same binaries 
; i.e. /usr/loca/bin that the shell normally would have. Yay Apple!
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-copy-env "PYTHONPATH")
  (exec-path-from-shell-initialize))

; Set spelling program
(setq ispell-program-name "aspell")

(require 'symbol-overlay)
(global-set-key [(control f3)] 'symbol-overlay-put)
(global-set-key (kbd "C-c h h") 'symbol-overlay-put)
(global-set-key [f3] 'symbol-overlay-switch-forward)
(global-set-key [(shift f3)] 'symbol-overlay-switch-backward)
(global-set-key [(meta f3)] 'symbol-overlay-remove-all)

; Allow for side-by-side diff viewing
(require 'diffview)

; Faster than ag, rg!
(require 'wgrep)
(require 'ripgrep)
(require 'rg)
(autoload 'wgrep-rg-setup "wgrep-rg")
(add-hook 'rg-mode-hook 'wgrep-rg-setup)

(require 'fd-dired)

(require 'transient)
(setq transient-levels-file '"~/Git/lightweight-emacs/transient/levels.el")
(setq transient-values-file '"~/Git/lightweight-emacs/transient/values.el")
(setq transient-history-file '"~/Git/lightweight-emacs/transient/history.el")

; Binding for line wrapping
(global-set-key (kbd "C-M-S-w") 'visual-line-mode)

(setq imenu-max-item-length 255)

; Add convenience function for killing all non-visible buffers. Very useful
; to avoid eglot starting too many servers that are no longer required.
(defun kill-all-nonvisible-buffers ()
  "Kill all buffers not currently shown in a window somewhere."
  (interactive)
  (dolist (buf  (buffer-list))
    (unless (get-buffer-window buf 'visible) (kill-buffer buf))))

(require 'eglot)

(global-set-key (kbd "C-M-S-e") #'eglot)
(global-set-key (kbd "C-M-S-E") #'eglot-shutdown)

; Disable formatting as you type.
(add-to-list 'eglot-ignored-server-capabilites :documentOnTypeFormattingProvider)

; Using in-memory PCH storage for perf and also no need to cleanup when `clangd` crashes. Setting unlimited file rename limit as well.
(add-to-list 'eglot-server-programs '((c++-mode c++-ts-mode c-mode c-ts-mode objc-mode cuda-mode) .
                                      ("clangd"
                                       "--background-index"
                                       "--background-index-priority=normal"
                                       "--clang-tidy"
                                       "--completion-style=detailed"
                                       "--completion-parse=auto"
                                       "--enable-config"
                                       "--experimental-modules-support"
                                       "--function-arg-placeholders=1"
                                       "--header-insertion=iwyu"
                                       "--header-insertion-decorators"
                                       "--import-insertions"
                                       "--limit-references=1000"
                                       "--limit-results=1000"
                                       "--log=info"
                                       "--pch-storage=memory"
                                       "--ranking-model=decision_forest"
                                       "--rename-file-limit=1000"
                                       "--use-dirty-headers"
                                       ;"-j"
                                       ;"32"
                                       )))
(add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio"))) ; Force Python to use pyright

(when lightweight-win32
  (add-to-list 'eglot-server-programs
               `(verse-mode . ("S:/source/repos/epic/ysiew_devvk/Engine/Restricted/NotForLicensees/Binaries/Win64/uLangServer-Win64-Debug.exe" ""))))

(when lightweight-aquamacs
(add-to-list 'eglot-server-programs '(swift-mode . ("/Applications/XcodeBeta.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")))
(add-to-list 'eglot-server-programs '(csharp-ts-mode . ("/usr/local/bin/omnisharp" "-lsp"))))

(when lightweight-linux
(add-to-list 'eglot-server-programs '(csharp--ts-mode . ("/usr/local/bin/omnisharp" "-lsp"))))

(when lightweight-win32
(add-to-list 'eglot-server-programs '(csharp-ts-mode . ("~/dist/omnisharp/OmniSharp.exe" "-lsp"))))

; TODO: move this to the p4 extensions module
(defun maybe-enable-eglot ()
  (unless (or (string-prefix-p "*P4 " (buffer-name))
              (bound-and-true-p ediff-minor-mode))
    (eglot-ensure)))

(defun disable-eglot-inlay-hints-in-p4-print ()
  "Disable Eglot inlay hints in *P4 ... buffers."
  (when (string-prefix-p "*P4 " (buffer-name))
    (eglot-inlay-hints-mode -1)))

(add-hook 'eglot-managed-mode-hook #'disable-eglot-inlay-hints-in-p4-print)

(add-hook 'c-mode-hook #'maybe-enable-eglot)
(add-hook 'c++-mode-hook #'maybe-enable-eglot)
(add-hook 'c-ts-mode-hook #'maybe-enable-eglot)
(add-hook 'c++-ts-mode-hook #'maybe-enable-eglot)

(add-hook 'objc-mode-hook #'maybe-enable-eglot)
; (add-hook 'csharp-mode-hook #'maybe-enable-eglot)
(add-hook 'swift-mode #'maybe-enable-eglot)
(add-hook 'haskell-mode #'maybe-enable-eglot)
(add-hook 'python-mode-hook #'maybe-enable-eglot)
(add-hook 'typescript-ts-mode-hook #'maybe-enable-eglot)

(setq eglot-autoshutdown t)
(setq eglot-autoreconnect 3)
(setq eglot-connect-timeout 30)
(setq eglot-advertise-cancellation t)
(setq eglot-sync-connect nil)
(setq eglot-extend-to-xref t)
(setq eglot-events-buffer-config (list :size 0 :format 'full))
(setq eglot-send-changes-idle-time 1.5)
(setq eglot-report-progress t)
(setq eglot-menu-string "Eg")
(setq eglot-inlay-hints-mode nil)

(require 'eglot-booster)
(with-eval-after-load 'eglot
           (require 'eglot-booster)
           (eglot-booster-mode))
(setq eglot-booster-io-only t)

(require 'consult-eglot)
(require 'consult-eglot-embark)
(with-eval-after-load 'embark
  (with-eval-after-load 'consult-eglot
    (require 'consult-eglot-embark)
    (consult-eglot-embark-mode)))

; Faster versions of building the imenu index using tree-sitter instead of eglot, which may be slow for larger projects.
(defun consult-treesit-imenu ()
  "Use `treesit-simple-imenu` if available, else fall back to default."
  (interactive)
  (let ((imenu-create-index-function
         (if (and (fboundp 'treesit-parser-list)
                  (treesit-parser-list))
             #'treesit-simple-imenu
           #'imenu-default-create-index-function)))
    (consult-imenu)))

(defun consult-treesit-imenu-multi ()
  "Use `treesit-simple-imenu` if available, else fall back to default."
  (interactive)
  (let ((imenu-create-index-function
         (if (and (fboundp 'treesit-parser-list)
                  (treesit-parser-list))
             #'treesit-simple-imenu
           #'imenu-default-create-index-function)))
    (consult-imenu-multi)))

(setq flymake-no-changes-timeout 1.0)

(setq global-eldoc-mode t)
(setq eldoc-idle-delay 0.5)
(setq eldoc-echo-area-prefer-doc-buffer t)
(setq eldoc-documentation-strategy 'eldoc-documentation-compose)
(setq eldoc-echo-area-use-multiline-p nil)

(global-set-key (kbd "M-RET") 'eglot-rename)
(global-set-key (kbd "C->") 'xref-find-definitions-other-window)
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key [C-down-mouse-1] 'xref-find-definitions-at-mouse)
(global-set-key (kbd "C-M-,") 'xref-go-forward)
(global-set-key (kbd "M-,") 'xref-go-back)
(global-set-key (kbd "C-c ?") 'eldoc-print-current-symbol-info)
(global-set-key (kbd "C-M-.") 'consult-eglot-symbols)

(require 'breadcrumb)
(breadcrumb-mode 1)

; Allow for peek window definition. This is a modified version of: https://tuhdo.github.io/emacs-frame-peek.html 
; that works with xref.
(defun xref-peek-definition ()
  "Peek at definition at point using xref."
  (interactive)
  (let ((func (lambda ()
                (call-interactively 'xref-find-definitions))))
    (make-peek-frame func)))

(defun make-peek-frame (find-definition-function &rest args)
  "Make a new frame for peeking definition"
    (let (summary
          doc-frame
          x y
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; 1. Find the absolute position of the current beginning of the symbol at point, ;;
          ;; in pixels.                                                                     ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (abs-pixel-pos (save-excursion
                           (beginning-of-thing 'symbol)
                           (window-absolute-pixel-position))))
      (setq x (car abs-pixel-pos))
      (setq y (+ (cdr abs-pixel-pos) (frame-char-height)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; 2. Create a new invisible frame, with the current buffer in it. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (setq doc-frame (make-frame '((minibuffer . nil)
                                    (name . "*xref Peek Definition*")
                                    (width . 120)
                                    (visibility . nil)
                                    (z-group . above)
                                    (height . 25))))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; 3. Position the new frame right under the beginning of the symbol at point. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (set-frame-position doc-frame x y)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; 4. Jump to the symbol at point. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (with-selected-frame doc-frame
        (apply find-definition-function args)
        ; (read-only-mode) ;TODO Find a way to make the frame only be read-only, not affect the buffer as well. Since that will affect other frames that also have the buffer open.
        ; (when semantic-stickyfunc-mode (semantic-stickyfunc-mode -1))
        (recenter-top-bottom))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; 5. Make frame visible again ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (make-frame-visible doc-frame)))

(global-set-key (kbd "C-x 6 .") 'xref-peek-definition)

(require 'bison-mode)

; Allow for manual-rescanning of buffers1
 (defun rescan-symbols-in-buffer()
   (interactive)
   (imenu--menubar-select imenu--rescan-item))

; Do not save semanticdb file to user home emacs directory
(setq semanticdb-default-save-directory "~/source/repos/lightweight-emacs/semantic-cache")

; Configure scrolling to only scroll half a page at a time
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
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-max-menu-items 1000)
(setq recentf-max-saved-items 10000)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
; (setq recentf-save-file (expand-file-name "recentf" "~/.emacs/recentf"))

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

; Function for reverting all open buffers
(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

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
(setq auto-save-list-file-prefix nil)
(setq delete-auto-save-files nil)

; Disable word wrapping by default
(set-default 'truncate-lines t)

; Undo functionality improved
(require 'undo-tree)
(global-undo-tree-mode)
; Remove the text from the modeline
(setq undo-tree-mode-lighter "")

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
(global-so-long-mode 1)

; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-screen t)

(setq compilation-directory-locked nil)
(scroll-bar-mode -1)
(setq shift-select-mode nil)
(setq enable-local-variables nil)

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
(when lightweight-linux or lightweight-aquamacs
  (setq compile-command "build.sh")
)
(when lightweight-win32
  (setq compile-command "build.bat")
)

; Force Emacs to always favour vertical split instead of horizontal split.
;(setq split-height-threshold nil   ; ⇒ nil means “don’t split horizontally”
;      split-width-threshold 0      ; ⇒ 0 means “OK to split vertically if >0 columns”
;      split-window-preferred-function 'split-window-sensibly)

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
(add-hook 'ediff-suspend-hook #'my-restore-pre-ediff-winconfig)

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

; Add support for Verse language
(require 'verse-mode)

; Highlight escape character sequences correctly
(custom-set-variables
 '(hes-mode-alist
   (quote
    ((c-mode . "\\(\\\\\\([0-7]\\{1,3\\}\\|x[[:xdigit:]]+\\|u[[:xdigit:]]\\{4\\}\\|U[[:xdigit:]]\\{8\\}\\|[\"'?\\abfnrtv]\\)\\)")
     (c-ts-mode . "\\(\\\\\\([0-7]\\{1,3\\}\\|x[[:xdigit:]]+\\|u[[:xdigit:]]\\{4\\}\\|U[[:xdigit:]]\\{8\\}\\|[\"'?\\abfnrtv]\\)\\)")
     (c++-mode . "\\(\\\\\\([0-7]\\{1,3\\}\\|x[[:xdigit:]]+\\|u[[:xdigit:]]\\{4\\}\\|U[[:xdigit:]]\\{8\\}\\|[\"'?\\abfnrtv]\\)\\)")
     (c++-ts-mode . "\\(\\\\\\([0-7]\\{1,3\\}\\|x[[:xdigit:]]+\\|u[[:xdigit:]]\\{4\\}\\|U[[:xdigit:]]\\{8\\}\\|[\"'?\\abfnrtv]\\)\\)")
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
         ("\\.xml$" . xml-mode)
         ("\\.natvis$" . xml-mode)
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
                          160 164 168 172 176 180 184 188 192 196 200))

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
  "Set margins in current buffer."
  (setq left-margin-width 4)
  (setq right-margin-width 4)
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
(global-set-key (kbd "C-x >") #'(lambda ()(interactive)(scroll-left 15)))
(global-set-key (kbd "C-x <") #'(lambda ()(interactive)(scroll-right 15)))

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
(setq mouse-wheel-tilt-scroll t)
(global-set-key (kbd "<S-wheel-down>") #'(lambda nil (interactive) (scroll-right 15)))
(global-set-key (kbd "<S-double-wheel-down>") #'(lambda nil (interactive) (scroll-right 15)))
(global-set-key (kbd "<S-triple-wheel-down>") #'(lambda nil (interactive) (scroll-right 15)))
(global-set-key (kbd "<S-mouse-4>") #'(lambda nil (interactive) (scroll-right 15)))
(global-set-key (kbd "<wheel-right>") #'(lambda nil (interactive) (scroll-right 15)))
(global-set-key (kbd "<wheel-left>") #'(lambda nil (interactive) (scroll-left 15)))
(global-set-key (kbd "<S-wheel-up>") #'(lambda nil (interactive) (scroll-left 15)))
(global-set-key (kbd "<S-double-wheel-up>") #'(lambda nil (interactive) (scroll-left 15)))
(global-set-key (kbd "<S-triple-wheel-up>") #'(lambda nil (interactive) (scroll-left 15)))
(global-set-key (kbd "<S-mouse-5>") #'(lambda nil (interactive) (scroll-left 15)))


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

; Additional keybinds for moving lines up/down on the home row
(require 'move-text)
(global-set-key (kbd "C-S-p") 'move-text-up)
(global-set-key (kbd "C-S-n") 'move-text-down)

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
; (setq compilation-context-lines 0)
; (setq compilation-error-regexp-alist
;     (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
;      compilation-error-regexp-alist))

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
    (set-variable 'grep-command "findstr -s -n -i -l ")
    (setq find-program "c:/msys64/usr/bin/find.exe"))

; Smooth scroll
(setq scroll-step 3)

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

(setq markdown-fontify-code-blocks-natively t)
(setq markdown-command "pandoc --from gfm -t html5 --mathjax --highlight-style pygments")

;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; scroll 3 lines at a time when using mwheel
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(pixel-scroll-precision-mode 1) ; Smooth scrolling with mouse/trackpad

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

(setq delete-old-versions :other)
(setq imenu-auto-rescan t)
(setq imenu-auto-rescan-maxout 500000)
(setq kept-new-versions 5)
(setq kept-old-versions 5)
(setq make-backup-file-name-function :ignore)
(setq make-backup-files nil)
(setq vc-handled-backends nil)

; Check if running on Macbook based off hostname and set the font size accordingly
; (if (string-equal system-name "sonictk-mbp.local")
;     (progn 
;         (add-to-list 'default-frame-alist '(font . "Liberation Mono-14"))
;         (set-face-attribute 'default nil :font "Liberation Mono-14"))
;     (progn
;         (add-to-list 'default-frame-alist '(font . "Liberation Mono-11.5"))
;         (set-face-attribute 'default t :font "Liberation Mono-11.5"))
; )

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

(with-eval-after-load 'zenburn-theme
  (custom-set-faces
   '(font-lock-warning-face ((t (:foreground "pink" :underline t :slant italic :weight bold))))
   '(hes-escape-backslash-face ((t (:foreground "tan" :slant italic :weight bold))))
   '(hes-escape-sequence-face ((t (:foreground "tan" :slant italic :weight bold))))
   '(hi-blue-b ((t (:foreground "sandy brown" :weight bold))))
   '(markdown-code-face ((t nil))) ; Don't want special face for Markdown syntax.

   '(linum-face ((t (:foreground "peru" :background unspecified))))
   ; Zenburn theme has some really weird background colours when visualizing whitespace, so we override them.
   '(whitespace-space ((t (:foreground "gray37" :background unspecified))))
   '(whitespace-hspace ((t (:foreground "gray37" :background unspecified))))
   '(whitespace-tab ((t (:foreground "gray37" :background unspecified))))
   '(whitespace-newline ((t (:foreground "gray37" :background unspecified))))
   '(whitespace-trailing ((t (:foreground "gray37" :background unspecified))))
   '(whitespace-line ((t (:foreground "gray37" :background unspecified))))
   '(whitespace-space-before-tab ((t (:foreground "gray37" :background unspecified))))
   '(whitespace-indentation ((t (:foreground "gray37" :background unspecified))))
   '(whitespace-empty ((t (:foreground "gray37" :background unspecified))))
   '(whitespace-space-after-tab ((t (:foreground "gray37" :background unspecified))))))

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

(require 'python-black)

; Use js-mode to highlight JSON files
(add-to-list 'auto-mode-alist '("\\.json$" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-ts-mode))
; Support TypeScript as well
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-ts-mode))

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
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-ts-mode))

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
      desktop-restore-eager       6 ; Number of buffers to restore immediately; rest are lazily loaded when emacs is idle
      desktop-lazy-verbose        nil
      desktop-lazy-idle-delay     5 
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop t
      desktop-auto-save-timeout   30)
(desktop-save-mode 1)
(setq desktop-minor-mode-table '((auto-fill-function auto-fill-mode)
                                 (defining-kbd-macro nil)
                                 (isearch-mode nil)
                                 (vc-mode nil)
                                 (vc-dired-mode nil)
                                 (erc-track-minor-mode nil)
                                 (global-whitespace-mode nil)
                                 (global-smartparens-mode nil))) ; Prevent desktop read from being slow

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
          #'(lambda ()
             (local-set-key (kbd "C-c u") 'string-inflection-ruby-style-cycle)))
;; for java
(add-hook 'java-mode-hook
          #'(lambda ()
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

; Scroll output in the compilation window automatically
(setq compilation-scroll-output t)

; Make GC not happen too much for unicode buffers 
(setq inhibit-compacting-font-caches t)

(defun select-next-window ()
  (other-window 1))

; Allow for completions when using interactive search
(eval-after-load "isearch"
  '(progn
     (require 'isearch-dabbrev)
     (define-key isearch-mode-map (kbd "M-/") 'isearch-dabbrev-expand))
)

; Load Epic Games specific stuff
(require 'epic-games-internal)

; Disable garbage collection while the minibuffer is open
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 100000000)) ; Follows spacemacs default of 100 MB

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

; Aliases for unintuitive commands
(defalias 'refresh-syntax-highlighting 'font-lock-fontify-buffer)

(require 'git-extensions)

; Perforce support
(require 'p4)
(require 'p4-extensions)
(setq p4-do-find-file nil) ; prevents p4 from taking ownership of a P4 file when it is loaded
(setq p4-auto-refresh nil)
(setq p4-check-empty-diffs t)
(setq p4-follow-symlinks t)
(setq p4-open-in-changelist t)
(setq p4-cleanup-time 10)
(defun p4-tramp-workaround-find-file-hook ()
    "do not let p4.el process remote TRAMP buffers"
    (when
        (and (fboundp 'tramp-tramp-file-p)
             (not (tramp-tramp-file-p buffer-file-name)))
      (p4-update-status)))

;; p4.el adds p4-update-status to find-file-hook
;; we replace it with a wrapper that filters out remote buffers.
(remove-hook 'find-file-hook 'p4-update-status)
(add-hook 'find-file-hooks 'p4-tramp-workaround-find-file-hook)

(defun my-p4-form-mode-hook ()
  "Custom hook to enable whitespace-mode and visual-line-mode in p4-change-form-mode."
  (whitespace-mode 1)
  (setq tab-width 4)
  (indent-tabs-mode t)
  (visual-line-mode 1))
(add-hook 'p4-form-mode-hook 'my-p4-form-mode-hook)

(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Window is currently dedicated to its buffer."
     "%s Window is currently not dedicated.")
   (current-buffer)))

(global-set-key (kbd "C-c d") 'toggle-window-dedicated)

; Allow for switching between visible windows with Shift + arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(require 'olivetti)

; Popper for nicer UX of "floating" windows
(require 'popper)
(setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        "\\*P4 shelve.+"
        "\\*P4 revert.+"
        "\\*P4 opened.+"
        "\\*P4 resolve.+"
        "\\*P4 add.+"
        "\\*P4 unshelve.+"
        "^\\*eldoc\\*$" eldoc-mode ;eldoc-mode as a popup
        help-mode
        compilation-mode))
(global-set-key (kbd "C-`") 'popper-toggle-latest)  
(global-set-key (kbd "M-`") 'popper-cycle)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)
(require 'popper-echo)

; Set the default font for everything
(add-to-list 'default-frame-alist '(font . "Fira Code Retina-12"))

; Cleanup and theme setup
(defun post-load-stuff ()
  (interactive)
  (menu-bar-mode -1)
  (load-theme 'zenburn t)
  (set-cursor-color "#40FF40")
  (set-face-background 'hl-line "#1a3a3a")
  ;(set-face-attribute 'default nil :family "Liberation Mono" :height 130 :weight 'normal :width 'normal)
  (if (eq system-type 'windows-nt)
    (w32-send-sys-command 61488) ; Maximize window
  )
  (recentf-load-list)
  (global-company-mode t)
  (setq fill-column 81)
  (popper-mode +1)
  (popper-echo-mode +1)
  (yas-global-mode 1)
)
(add-hook 'window-setup-hook 'post-load-stuff t)
