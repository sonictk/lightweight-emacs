;;; mel-mode.el --- MEL mode
;; Time-stamp: <2005-12-21 11:07:51 narazaki>
;; Version: 0.8.1
 
;; Copyright (C) 2001-2006 by Free Software Foundation, Inc.
;; Author: Shuji Narazaki <narazaki@cs.cis.nagasaki-u.ac.jp>
;; Keywords: languages

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;; How to use:
;;; 1. add the following two lines to your ~/.emacs or default.el
;;;  (add-to-list 'auto-mode-alist '("\\.mel$" . mel-mode))
;;;  (autoload 'mel-mode "mel-mode" nil t)
;;; 2. add the appropriate definition of mel-mode-document-base in ~/.emacs
;;;    This variable is used for opening online document
;;;    set to the directory name in which GlobalIndex.html is located
;;;    For example, I uses:
;;;  (setq mel-mode-document-base "/home/narazaki/Lib/Maya/html/ja_JP/")
;;;    Note: If you use this command, you must set variables in
;;;    browse-url.el to open appropriate WWW browser from Emacs.
;;;    (see the definition of mel-mode-document-base below). 
;;;    WARNING: NOT TESTED ON WINDOWS/MAC OS/IRIX

;;; Thanks:
;;;   Martin Vogt (various help to fix bugs on Xemacs)

;;; TODO
;;; o syntax-oriented automatic indentation by smart way
;;; o popup tooltip to display the long name on a variable in short name style

;;; Code:
(require 'font-lock)
(require 'cc-mode)
(require 'browse-url)

(defconst mel-mode-version "0.8.1")

(defvar mel-mode-document-base "/usr/local/share/maya/html/"
  "Directory name(string) of the Maya online document.
Set to the directory in which GlobalIndex.html is located.
For example, if you have /home/myname/Maya/html/en_US/MasterIndex.html,
then the value of this variable should be:
\"/home/myname/Maya/html/en_US/\"
Note: you need not to add \"file://\" at the beginning. And please it should
end with slash")

(if (boundp 'mel-mode-map)
    nil
  (defvar mel-mode-map (make-sparse-keymap))
  (set-keymap-parent mel-mode-map c++-mode-map)
  (define-key mel-mode-map "\C-cf" 'mel-mode-open-document)
  )
(defvar mel-mode-abbrev-table nil)
(defvar mel-mode-syntax-table nil)
(defvar mel-mode-hook '(turn-on-font-lock))

(defun mel-mode ()
  "a major mode for MEL script file"
  (interactive)
  ;; easy but duty way to prepare the mode
  (c++-mode)
  (setq major-mode 'mel-mode)
  (setq mode-name "MEL")
  (setq local-abbrev-table mel-mode-abbrev-table)
  (set-syntax-table c++-mode-syntax-table)
  (use-local-map mel-mode-map)
  (setq abbrev-mode t)
  ;; Run the hooks.
  (run-hooks 'mel-mode-hook))

(defvar mel-mode-search-keyword-history nil)

(defun mel-mode-open-document (keyword)
  (interactive 
   (list (let* ((default (current-word))
		(input (read-string "Keyword: "
				    default
				    'mel-mode-search-keyword-history)))
	   (if (string= input "")
	       (error "No function name given")
	     input))))
(message keyword)
(browse-url (mel-mode-command-name-to-url keyword)))

(defun mel-mode-command-name-to-url (name)
  (let ((protocol "file://")
	(dir-delimiter "/")
	(command-directory "Commands")
	(html-prefix ".html"))
  (concat protocol
	  mel-mode-document-base
	  command-directory
	  dir-delimiter
	  name
	  html-prefix)))

;; font-lock definitions
;; adding additional types: matrix, string, vector (See UserGuide/Mel/Mel.htm)
(setq mel-mode-font-lock-keywords-3
      '(("^\\(\\(\\(\\sw\\|\\s_\\|[:~*&]\\)+[ 	]+\\)\\(\\(\\(\\sw\\|\\s_\\|[:~*&]\\)+[ 	]+\\)\\(\\(\\sw\\|\\s_\\|[:~*&]\\)+[ 	]+\\)?\\)?\\)?\\([*&]+[ 	]*\\)?\\(\\(\\sw\\|\\s_\\|[:~*&]\\)+\\)[ 	]*(" 
	 10 font-lock-function-name-face)
	("^\\(typedef[ 	]+struct\\|struct\\|static[ 	]+struct\\)[ 	]+\\(\\(\\sw\\|\\s_\\|[:~*&]\\)+\\)[ 	]*\\({\\|$\\)"
	 2 font-lock-function-name-face)
	("case[ 	]+\\(\\(\\sw\\|\\s_\\)+\\)[ 	]+:" . 1)
	("\\<\\(default\\):" . 1)
	("^#[ 	]*include[ 	]+\\(<[^>\"\n]+>\\)"
	 1 font-lock-string-face)
	("^#[ 	]*define[ 	]+\\(\\(\\sw+\\)(\\)"
	 2 font-lock-function-name-face)
	("^#[ 	]*if\\>"
	 ("\\<\\(defined\\)\\>[ 	]*(?\\(\\sw+\\)?" nil nil 
	  (1 font-lock-preprocessor-face)
	  (2 font-lock-variable-name-face nil t)))
	("^#[ 	]*elif\\>"
	 ("\\<\\(defined\\)\\>[ 	]*(?\\(\\sw+\\)?" nil nil 
	  (1 font-lock-preprocessor-face)
	  (2 font-lock-variable-name-face nil t)))
	("^\\(#[ 	]*[a-z]+\\)\\>[ 	]*\\(\\sw+\\)?" 
	 (1 font-lock-preprocessor-face) 
	 (2 font-lock-variable-name-face nil t))
	("^\\(\\sw+\\)\\(::\\(\\sw+\\)\\)?[ 	]*(" 
	 (1 (if (match-beginning 2)
		font-lock-type-face font-lock-function-name-face))
	 (3 (if (match-beginning 2) font-lock-function-name-face) nil t))
	("\\<\\(auto\\|bool\\|c\\(har\\|lass\\|o\\(mplex\\|nst\\)\\)\\|double\\|e\\(num\\|x\\(p\\(licit\\|ort\\)\\|tern\\)\\)\\|f\\(loat\\|riend\\)\\|in\\(line\\|t\\)\\|long\\|m\\(atrix\\|utable\\)\\|namespace\\|proc\\|register\\|s\\(hort\\|igned\\|t\\(atic\\|ring\\|ruct\\)\\)\\|t\\(emplate\\|ype\\(def\\|name\\)\\)\\|u\\(\\(n\\(ion\\|signed\\)\\|sing\\)\\)\\|v\\(ector\\|irtual\\|o\\(id\\|latile\\)\\)\\|wchar_t\\)\\>" . font-lock-type-face)
	("\\<\\(operator\\)\\>[ 	]*\\([][)(><!=+-][][)(><!=+-]?\\)?"
	 (1 font-lock-keyword-face)
	 (2 font-lock-function-name-face nil t))
	("\\<\\(case\\|goto\\)\\>[ 	]*\\([^ 	\n:;]+\\)?" 
	 (1 font-lock-keyword-face)
	 (2 font-lock-reference-face nil t))
	("^[ 	]*\\(\\sw+\\)[ 	]*:[^:]" 1 font-lock-reference-face)
	("\\<\\(a\\(nd\\(\\|_eq\\)\\|sm\\)\\|b\\(it\\(or\\|and\\)\\|reak\\)\\|c\\(atch\\|o\\(mpl\\|n\\(tinue\\|st_cast\\)\\)\\)\\|d\\(elete\\|o\\|ynamic_cast\\)\\|else\\|f\\(alse\\|or\\)\\|if\\|n\\(ew\\|ot\\(\\|_eq\\)\\)\\|p\\(r\\(ivate\\|otected\\)\\|ublic\\)\\|or\\(\\|_eq\\)\\|re\\(interpret_cast\\|turn\\)\\|s\\(izeof\\|tatic_cast\\|witch\\)\\|t\\(h\\(is\\|row\\)\\|r\\(ue\\|y\\)\\|ypeid\\)\\|xor\\(\\|_eq\\)\\|while\\)\\>" . font-lock-keyword-face)
	("\\<\\(auto\\|bool\\|c\\(har\\|lass\\|o\\(mplex\\|nst\\)\\)\\|double\\|e\\(num\\|x\\(p\\(licit\\|ort\\)\\|tern\\)\\)\\|f\\(loat\\|riend\\)\\|global\\|in\\(line\\|t\\)\\|long\\|m\\(atrix\\|utable\\)\\|namespace\\|proc\\|register\\|s\\(hort\\|igned\\|t\\(atic\\|\\|ring\\|ruct\\)\\)\\|t\\(emplate\\|ype\\(def\\|name\\)\\)\\|u\\(\\(n\\(ion\\|signed\\)\\|sing\\)\\)\\|v\\(ector\\|irtual\\|o\\(id\\|latile\\)\\)\\|wchar_t\\)\\>\\([ 	*&]+\\sw+\\>\\)*"
	 (font-lock-match-c++-style-declaration-item-and-skip-to-next
	  (goto-char (or (match-beginning 13) (match-end 1)))
	  (goto-char (match-end 1))
	  (1 (cond ((match-beginning 2) (quote font-lock-type-face))
		   ((match-beginning 4) (quote
					 font-lock-function-name-face))
		   (t (quote font-lock-variable-name-face))))
	  (3 (if (match-beginning 4)
		 (quote font-lock-function-name-face)
	       (quote font-lock-variable-name-face)) nil t)))
	;;; variable in MEL 
	("\\<\\(\\$\\sw+\\)"
	 . font-lock-variable-name-face)
	("\\(}\\)[ 	*]*\\sw"
	 (font-lock-match-c++-style-declaration-item-and-skip-to-next
	  (goto-char (match-end 1)) nil
	  (1 (if (match-beginning 4)
		 font-lock-function-name-face
	       font-lock-variable-name-face))))
	("^\\(\\sw+\\)\\>\\([ 	*]+\\sw+\\>\\)*"
	 (1 font-lock-type-face)
	 (font-lock-match-c++-style-declaration-item-and-skip-to-next 
	  (goto-char (or (match-beginning 2) (match-end 1))) nil 
	  (1 (cond ((match-beginning 2) (quote font-lock-type-face))
		   ((match-beginning 4) (quote font-lock-function-name-face))
		   (t (quote font-lock-variable-name-face))))
	  (3 (if (match-beginning 4)
		 (quote font-lock-function-name-face)
	       (quote font-lock-variable-name-face)) nil t)))))

(setq mel-font-lock-keywords
      (append '(("\\<\\(-[a-zA-Z][a-zA-Z0-9_]*\\|false\\|global\\|in\\|no\\|on\\|off\\|true\\|yes\\)\\>" . font-lock-keyword-face)
		)
	      c++-font-lock-keywords
	      c++-font-lock-keywords-1
	      c++-font-lock-keywords-2
	      mel-mode-font-lock-keywords-3
	      ))

(cond ((string-match "^GNU Emacs 2[12]" (emacs-version))
       (add-to-list 'font-lock-defaults-alist
		    (list 'mel-mode
			  '(mel-font-lock-keywords)
			  nil nil
			  '((?_ . "w") (?- . "w") (?$ . "w"))
			  'beginning-of-defun
			  '(font-lock-mark-block-function . mark-defun)))
       (font-lock-add-keywords 'mel-mode '("\\<\\(-[a-zA-Z]+\\)\\>")))
      ;; Xemacs features
      ((string-match "^XEmacs 21" (emacs-version))
       (put 'mel-mode
	    'font-lock-defaults
	    (list '(mel-font-lock-keywords)
		  nil			; keywords-only
		  nil			; case-fold
		  '((?_ . "w") (?- . "w") (?$ . "w"))
		  'beginning-of-defun))))

(or (rassoc 'mel-mode auto-mode-alist)
    (add-to-list 'auto-mode-alist '("\\.mel$" . mel-mode)))

(provide 'mel-mode)

;;; mel-mode.el ends here