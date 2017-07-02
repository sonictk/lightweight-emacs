;;; maxscript-mode.el --- 3dsmax script (maxscript) editing mode for GNU Emacs
;; -*- Mode:Emacs-Lisp -*-

;; Copyright (C) 2010 akm

;; Author:  akm <akm.gfx@gmail.com>
;; Version: 0.1

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

;; [Usage]
;; 
;;  (autoload 'maxscript-mode "maxscript-mode" "maxscript-mode" t)
;;  (setq auto-mode-alist
;;     (append '(("\\.ms$" . maxscript-mode)) auto-mode-alist))
;;

(require 'regexp-opt)

(defgroup maxscript-mode nil
  "Major mode for editing Maxscript script files."
  :group 'programming)

(defcustom maxscript-mode-hook nil
  "*List of hook functions run by `maxscript-mode' (see `run-hooks')"
  :type 'hook
  :group 'maxscript-mode)

(defvar maxscript-mode-abbrev-table nil
  "")
(define-abbrev-table 'maxscript-mode-abbrev-table ())

(defvar maxscript-mode-map nil
  "Keymap used in Maxscript mode buffers.")

(if maxscript-mode-map
    ()
  (setq maxscript-mode-map (make-sparse-keymap))
  (define-key maxscript-mode-map ")" 'maxscript-mode-electric-insert-close-brace)
  ;;  (define-key maxscript-mode-map "\C-c\C-d" 'maxscript-run-script)
  )

(defvar maxscript-indent-level 4 "The indentation level for Maxscript scripts.")

(defconst maxscript-keywords
  (eval-when-compile
    (regexp-opt
     '(
       "about" "and" "animate" "as" "at"
       "by"
       "case" "catch" "collect" "continue" "coordsys"
       "do"
       "else" "exit"
       "fn" "for" "from" "function"
       "global"
       "if" "in"
       "local"
       "macroscript" "mapped" "max"
       "not"
       "of" "off" "on" "or"
       "parameters" "persistent" "plugin"
       "rcmenu" "return" "rollout" "set" "struct"
       "then" "throw" "to" "tool" "try"
       "undo" "utility"
       "when" "where" "while" "with"
       )))
  "MAXScript keywords.")

(defconst maxscript-constants
  (eval-when-compile
    (regexp-opt
     '(
       "true" "false"
       "on" "off"
       "pi" "e"
       "red" "green" "blue" "white" "black" "orange" "yellow" "brown" "gray"
       "x_axis" "y_axis" "z_axis"
       "ok"
       "undefined"
       "unsupplied"
       "dontcollect"
       )))
  "MAXScript constants.")

(defconst maxscript-global-variables
  (eval-when-compile
    (regexp-opt
     '(
       "activeGrid" "ambientColor" "ambientColorController" "animationRange" "animButtonEnabled" "animButtonState" "autoBackup.enabled" "autoBackup.time"
       "backgroundColor" "backgroundColorController" "backgroundImageFileName"
       "cui.commandPanelOpen" "currentMaterialLibrary"
       "displayGamma"
       "fileInGamma" "fileOutGamma"
       "displaySafeFrames"
       "environmentMap"
       "flyOffTime" "frameRate"
       "globalTracks"
       "hardwareLockID" "hotspotAngleSeparation"
       "keyboard.shiftPressed" "keyboard.controlPressed" "keyboard.altPressed" "keyboard.escPressed"
       "lightTintColor" "lightTintColorController" "lightLevel" "lightLevelController" "listener " "localTime" "logsystem.quietmode"
       "macroRecorder" "manipulateMode" "maxFileName" "maxFilePath" "meditMaterials"
       "numEffects" "numAtmospherics" "numSubObjectLevels"
       "playActiveOnly" "preferences.constantReferenceSystem" "preferences.dontRepeatRefMsg" "preferences.flyOffTime" "preferences.InvalidateTMOpt" "preferences.maximumGBufferLayers" "preferences.spinnerWrap" "preferences.spinnerPrecision " "preferences.spinnerSnap" "preferences.useSpinnerSnap" "preferences.useLargeVertexDots" "preferences.useTransformGizmos" "preferences.useVertexDots"
       "realTimePlayback" "renderer" "renderDisplacements" "renderEffects" "renderHeight" "renderPixelAspect" "renderWidth" "rendOutputFilename" "rendSimplifyAreaLights" "rootNode"
       "sceneMaterials" "scriptsPath" "selectionSets" "showEndResult" "skipRenderedFrames" "sliderTime" "snapMode.active" "snapMode.type" "subObjectLevel" "sysInfo.DesktopSize" "sysInfo.DesktopBPP" "sysInfo.MAXPriority"
       "ticksPerFrame" "timeConfiguration.playActiveOnly" "timeConfiguration.realTimePlayback" "timeConfiguration.PlaybackSpeed" "timeConfiguration.PlaybackLoop" "timeConfiguration.useTrackBar" "toolMode.coordSysNode" "trackbar.filter" "trackbar.visible" "trackViewNodes"
       "units.DisplayType" "units.MetricType" "units.USType" "units.USFrac" "units.CustomName" "units.CustomValue" "units.CustomUnit" "units.SystemScale" "units.SystemType" "useEnvironmentMap"
       "videoPostTracks" "viewport.activeViewport" "viewport.numViews"
       "scanlineRender.antiAliasFilter" "scanlineRender.antiAliasFilterSize" "scanlineRender.enablePixelSampler"
       ;;---
       "currentTime" "editorFont" "editorFontSize" "editorTabWidth" "escapeEnable" "heapFree" "heapSize" "inputTextColor" "messageTextColor" "outputTextColor" "options.oldPrintStyles" "options.showGCStatus" "stackLimit" "?'"
       ;;--- collections
       "objects" "geometry" "lights" "cameras" "helpers" "shapes" "systems" "spacewarps" "selection" ;ObjectSet
       "selectionSets" ;SelectionSetArray
       "currentMaterialLibrary" "sceneMaterials" "meditMaterials" ;MaterialLibrary

       )))
  "MAXScript global variables.")


(defconst maxscript-font-lock-keywords
  (list
   '("\\(--.*$\\)"
     1 'font-lock-comment-face)
   '("\\(\"[^\"]*\"\\)"
     1 'font-lock-string-face)
   `(eval .
          (cons (concat "\\<\\(" ,maxscript-keywords "\\)\\>") 'font-lock-keyword-face))
   `(eval .
          (cons (concat "\\<\\(" ,maxscript-constants "\\)\\>") 'font-lock-constant-face))
   `(eval .
          (cons (concat "\\<\\(" ,maxscript-global-variables "\\)\\>") 'font-lock-variable-name-face))
   ))


(defun maxscript-mode-calc-indent-level ()
  "Calculate the indent level for the current line."
  (save-excursion
    (let* ((indent-data (parse-partial-sexp (point-min)
                                            (line-beginning-position)))
           (indent (car indent-data))
           (in-comment (nth 4 indent-data))
           close-block
           close-comment
           pos)
      (back-to-indentation)
      (setq
       first-char (buffer-substring (point) (+ 1 (point)))
       close-block (looking-at ")")
       )
      (when close-block
        (setq indent (1- indent)))
      (setq pos (* indent maxscript-indent-level))
      )
    ))


(defun maxscript-indent-line (&optional indent)
  "Indents the current line."
  (interactive)
  (let ((indent (or indent (maxscript-mode-calc-indent-level)))
        pos)
    (save-excursion
      (back-to-indentation)
      (delete-region (point-at-bol) (point))
      (indent-to indent)
      (setq pos (point)))
    (when (> pos (point))
      (goto-char pos))))

(defun maxscript-mode-electric-insert-close-brace ()
  "Insert a closing brace }."
  (interactive)
  (insert ")")
  (maxscript-indent-line)
  )


(defun maxscript-mode ()
  "Major mode for editing Maxscript script files."
  (interactive)
  (kill-all-local-variables)
  (use-local-map maxscript-mode-map)
  (setq local-abbrev-table maxscript-mode-abbrev-table)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'maxscript-indent-line)

  (setq font-lock-defaults
        '((maxscript-font-lock-keywords)
          t
          nil
          ((?_ . "w") (?~ . "w"))
          nil
          ))

  (setq mode-name (concat "MAXScript"))
  (setq major-mode 'maxscript-mode)

  (run-hooks 'maxscript-mode-hook)
  )

;; (defun maxscript-run-script ()
;;   (interactive)
;;   (let ((w32-start-process-show-window t))
;;     (apply (function start-process)
;;            "runscript.js" nil "wscript" (list "runscript.js" buffer-file-name)))
;;   )

(provide 'maxscript-mode)
