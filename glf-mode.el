;; -*- lexical-binding: t -*-

;;; glf-mode.el -- major mode for viewing GLF log files
;;
;; Copyright (C) 2011-2013 Christophe Mealares
;;
;;; Authors:
;;     Christophe Mealares
;;     Laurent P
;;

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Main features:
;;
;;    Syntax coloration
;;    Focus on a particular thread by hiding others
;;    Thread navigation:
;;      - move to next/previous thread paragraph;
;;      - move to next/previous paragraph in same thread.
;;    Collapse/expand paragraph
;;    Indent nested scopes
;;    Goto xml trace file
;;    Goto source code

;;; Installation:
;;
;;    Put this file on your load path.
;;    Then add the following lines to your Emacs initialization file:
;;
;;      (add-to-list 'auto-mode-alist '("\\.glf$" . glf-mode))
;;      (autoload 'glf-mode "glf" nil t)
;;
;;    Performance is better when this file is byte-compiled.
;;    Byte-compile it by typing:
;;      M-x byte-compile-file and have Emacs byte-compile


;;;
;;; Customization
;;;

(defgroup glf nil
  "Major mode for viewing GLF files."
  :prefix "glf-"
  :group 'data
  :version "1.0")

(defcustom glf-location-visibility-mode 'grayed-out
  "Default style of the file location."
  :type '(choice (const :tag "Visible" :value visible)
                 (const :tag "Grayed out" :value grayed-out))
  :group 'glf)

(defcustom glf-thread-colorization t
  "Non nil means to color different threads with different colors."
  :type 'boolean
  :group 'glf)

;;;
;;; Faces
;;;

(defgroup glf-faces nil
  "Fontification colors."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "glf-"
  :group 'glf)

(defface glf-light-text
  '((((class color) (background light)) (:foreground "Grey80"))
    (((class color) (background dark)) (:foreground "Grey20")))
  "Text almost invisible")

(defcustom glf-light-text-face 'glf-light-text
  "Face for uninteresting text."
  :type 'face
  :group 'glf-faces)

(defcustom glf-filename-face 'font-lock-function-name-face
  "Face for file names."
  :type 'face
  :group 'glf-faces)

(defcustom glf-line-number-face 'font-lock-variable-name-face
  "Face for line numbers."
  :type 'face
  :group 'glf-faces)

(defcustom glf-column-time-face 'font-lock-type-face
  "Face for column Time"
  :type 'face
  :group 'glf-faces)

(defcustom glf-column-process-id-face 'font-lock-builtin-face
  "Face for column ProcessID"
  :type 'face
  :group 'glf-faces)

(defcustom glf-column-thread-id-face 'font-lock-builtin-face
  "Face for column ThreadID"
  :type 'face
  :group 'glf-faces)

(defcustom glf-column-scope-tag-face 'font-lock-keyword-face
  "Face for column ScopeTag"
  :type 'face
  :group 'glf-faces)

(defcustom glf-column-text-face 'font-lock-doc-face
  "Face for column ScopeTag"
  :type 'face
  :group 'glf-faces)

(defcustom glf-warnings-face 'font-lock-comment-face
  "Face for warnings"
  :type 'face
  :group 'glf-faces)

(defcustom glf-errors-face 'font-lock-warning-face
  "Face for errors"
  :type 'face
  :group 'glf-faces)

;;;
;;; Syntax highlighting
;;;

(defconst glf-background-colors
  '("lavender"
    "misty rose"
    "lemon chiffon"
    "seashell"
    "sky blue"
    "pale green"
    "linen"
    "alice blue"
    "ivory"
    "bisque"
    "ghost white"
    "gainsboro"
    "aquamarine"
    "dark sea green"
    "khaki"
    "tan"
    "rosy brown"
    "plum"
    "pink"
    "cornsilk")
  "Colors for backgrounds of threads.")

(defun glf-make-thread-face (tid)
  "Define a face for thread hilighting."
  (or (gethash tid glf-thread-faces-map)
      (let* ((color (nth glf-next-background-color glf-background-colors))
             (facename (make-symbol (concat "glf-" (subst-char-in-string ?\s ?- color) "-face")))
             (face (make-face facename)))
        (when (not (face-background face))
          (setq glf-next-background-color (1+ glf-next-background-color))
          (when (>= glf-next-background-color (length glf-background-colors))
            (setq glf-next-background-color 0))
          (set-face-background face color))
        (puthash tid face glf-thread-faces-map))))

(defun glf-get-current-thread-face ()
  "Return the face of the thread on the current line"
  (if glf-thread-colorization
      (let ((tid (match-string-no-properties glf-thread-match-data-index)))
        (if tid
            (glf-make-thread-face tid)
          glf-column-text-face))
    glf-column-text-face))

(defun glf-compute-font-lock-column-specification ()
  (let
      ((colspec '(("Time" . glf-column-time-face)
                  ("Importance" . nil)
                  ("Severity" . nil)
                  ("Exception" . nil)
                  ("ProcessID" . glf-column-process-id-face)
                  ("ThreadID" . glf-column-thread-id-face)
                  ("ScopeTag" . glf-column-scope-tag-face)
                  ("Text" . (glf-get-current-thread-face)) ))
       (lockspec '()))

    (dolist (column glf-columns)
      (let ((def (assoc column colspec)))
        (push (if (null def) :invisible (cdr def))
              lockspec)))

    (setq lockspec (nreverse lockspec))
    (glf-group-columns (car lockspec) 1 (cdr lockspec))))

(defun glf-group-columns (element count list)
  (cond
   ((null list)
    (list (cons count element)))
   ((and (equal element (car list))
         (or (null (car list))
             (eq :invisible (car list))))
    (glf-group-columns element (1+ count) (cdr list)))
   (t
    (cons (cons count element)
          (glf-group-columns (car list) 1 (cdr list))))))

(defun glf-compute-thread-match-data-index ()
  "Give the number of the match data of the ThreadID column. This column must exist."
  (let ((tidIndex (gethash "ThreadID" glf-column-indexes-map))
        (i 0)
        (colspec glf-font-lock-column-specification))

    (if (not tidIndex)
        (error "Unknown column name: ThreadIndex")

      (while (and colspec
                  (> tidIndex 0))

        (let ((colCount (caar colspec)))
          (setq tidIndex (- tidIndex colCount))
          (setq i (1+ i))
          (setq colspec (cdr colspec))) )
      i)))

(defun glf-compute-font-lock-column-faces ()
  (let ((res nil)
        (index 1))

    (dolist (spec glf-font-lock-column-specification (nreverse res))
      (let ((font (cdr spec))
            (invisible-face '(list 'face 'default 'invisible t))
	    ;;(invisible-face 'glf-light-text-face) ; for debug
            )
        (when font
          (setq res (cons (list index
                                (if (eq font :invisible) invisible-face font))
                          res))
          (setq index (1+ index)))))))

(defun glf-column-matcher (search-limit)
  (let
      ((matchlist (search-columns glf-font-lock-column-specification search-limit)))
    (set-match-data matchlist)
    ;; return nil for failure, t for success
    matchlist))

(defun search-columns (speclist search-limit)
  (if (not (re-search-forward "^|" search-limit t))
      nil
    (catch 'badmatch
      (backward-char 1)
      (let ((matchlist (list search-limit (point))))
        (dolist (spec speclist)
          (let ((count (car spec))
		(face (cdr spec)))
	    (if (null face)
		(skip-column search-limit count)
	      (let ((match (search-column search-limit count (eq face :invisible))))
		(when match
		  (setq matchlist (cons (cadr match) (cons (car match) matchlist))))))))
        (nreverse matchlist)))))

(defun search-column (search-limit count isInvisible)
  (let
      ((begin (if isInvisible (point) (1+ (point))))
       (end (re-search-forward glf-column-pattern search-limit t count)))
    (unless end (throw 'badmatch nil))
    (list begin end)))

(defun skip-column (search-limit count)
  (unless (re-search-forward glf-column-pattern search-limit t count)
    (throw 'badmatch nil)))

(defun glf-font-lock-keywords ()
  "Build font lock keywords for `glf-mode'."
  (let ((sep glf-column-separator))
    (list

     ;; Location
     (if (eq glf-location-visibility-mode 'grayed-out)
         (cons (format "^[^%c][^:\r\n]+:[[:digit:]]+:.*\r?" glf-column-separator) 'glf-light-text-face)
       (cons glf-location-pattern '((1 glf-filename-face) (2 glf-line-number-face))))

     ; Columns
     (cons 'glf-column-matcher (glf-compute-font-lock-column-faces))

     ;; Errors and exceptions
     (cons (format "%c\\([AEX]\\(?:|X\\)?\\)%c.*%c\\([^%c\r\n]*\\)$" sep sep sep sep) '((1 glf-errors-face) (2 glf-errors-face append)))
     ;; Warnings
     (cons (format "%c\\(W\\)%c.*%c\\([^%c\r\n]*\\)$" sep sep sep sep) '((1 glf-warnings-face) (2 glf-warnings-face append)))
     )) )

;;;
;;; Parsing file header
;;;

(defun glf-read-header-alist ()
  (let ((header-alist ()))
    (goto-char (point-min))
    (if (not (looking-at-p "FILE_TYPE:"))
      (message "Broken GLF file: header not found")
      (while (looking-at "\\([A-Z_]+\\):\\(.*\\)")
        (let ((name (match-string-no-properties 1))
              (value (match-string-no-properties 2)))
          (push (cons name value) header-alist)
          (forward-line) )))

    (when (looking-at-p "HEADER_END")
      (forward-line))
    header-alist))

(defun glf-get-header-value (name type alist default)
  (let* ((def (assoc name alist))
         (strval  (when def (cdr def))))
    (cond
     ((eq type :string) (if (null strval) default strval))
     ((eq type :char)  (if (null strval) default (string-to-number strval)))
     (t (error "%s: invalid field type for field %s" (symbol-name type) name)))))

(defun glf-parse-header ()
  (save-excursion
    (let ((header (glf-read-header-alist)))

      (set (make-local-variable 'glf-end-of-header-point) (point))
      (set (make-local-variable 'glf-file-encoding) (glf-get-header-value "ENCODING" :string header "UTF-8"))
      (set (make-local-variable 'glf-record-separator) (glf-get-header-value "RECORD_SEPARATOR" :char header 30))
      (set (make-local-variable 'glf-column-separator) (glf-get-header-value "COLUMN_SEPARATOR" :char header 124))
      (set (make-local-variable 'glf-escape-char) (glf-get-header-value "ESC_CHARACTER" :char header 27))

      (let ((cols (glf-get-header-value "COLUMNS" :string header "Location|Guid|Time|Tzone|Trace|Log|Importance|Severity|Exception|DeviceName|ProcessID|ThreadID|ThreadName|ScopeTag|MajorTick|MinorTick|MajorDepth|MinorDepth|RootName|RootID|CallerName|CallerID|CalleeName|CalleeID|ActionID|DSRRootContextID|DSRTransaction|DSRConnection|DSRCounter|User|ArchitectComponent|DeveloperComponent|Administrator|Unit|CSNComponent|Text")))

        ;; remove first column because it is the location and is not a real column
        (set (make-local-variable 'glf-columns) (cdr (split-string cols (char-to-string glf-column-separator))))

        (let ((nb-columns (length glf-columns)))
          (set (make-local-variable 'glf-column-indexes-map) (make-hash-table :test 'equal :weakness t :size nb-columns))
          (dotimes (i nb-columns)
            (puthash (nth i glf-columns) i glf-column-indexes-map))))
      )))

(defun glf-test ()
  "Print the result of some reading functions"
  (interactive)
  (with-output-to-temp-buffer "*glf-test*"

    (print "*** Variables")
    (princ (format "glf-file-encoding : %s\n" glf-file-encoding))
    (princ (format "glf-record-separator : %s\n" glf-record-separator))
    (princ (format "glf-column-separator : %s\n" glf-column-separator))
    (princ (format "glf-escape-char : %s\n" glf-escape-char))
    (princ (format "glf-columns : %s\n" glf-columns))
    (princ (format "glf-column-indexes-map : %s\n" glf-column-indexes-map))
    (princ (format "glf-font-lock-column-specification : %s\n" glf-font-lock-column-specification))
    (princ (format "glf-thread-match-data-index : %d\n" glf-thread-match-data-index))
))

;;;
;;; Movements
;;;

(defsubst glf-get-index (column-name)
  (let ((index (gethash column-name glf-column-indexes-map)))
    (if index
	index
      (error "Unknown column name: %s" column-name))))

(defsubst glf-goto-field (n)
  "Move to nth field of the current infoline. Counting starts at 0."
  (beginning-of-line)
  (if (zerop n)
      t
    (search-forward (format "%c" glf-column-separator) (line-end-position) t (1+ n))))

(defun glf-read-column (col-index)
  "Read column value on current column line. Caller must set position on a column line."
  (save-excursion
    (beginning-of-line)

    (let ((separator (format "%c" glf-column-separator)))
        (let
            ((start-pos (search-forward separator (line-end-position) t (1+ col-index)))
             (end-pos (search-forward separator (line-end-position) t 1)))
          (if (null start-pos)
              nil
            (buffer-substring-no-properties start-pos
                                            (1- (if (null end-pos) (line-end-position) end-pos)))) ))))

(defun glf-search-error (search-fun)
  (unless (apply search-fun
                 (list (format "%c[AEX]%c" glf-column-separator glf-column-separator) nil t))
    (message "No more error")))

(defun glf-next-error ()
  "Search for next error."
  (interactive)
  (glf-search-error 're-search-forward))

(defun glf-previous-error ()
  "Search for previous error."
  (interactive)
  (glf-search-error 're-search-backward))

(defun glf-forward-infoline ()
  "Move to next infoline"
  (beginning-of-line)
  (forward-line)
  (while (and (not (eobp))
              (not (looking-at glf-infoline-pattern)))
    (forward-line)))

(defun glf-backward-infoline ()
  "Move to previous infoline"
  (beginning-of-line)
  (forward-line -1)
  (while (and (not (bobp))
              (not (looking-at glf-infoline-pattern)))
    (forward-line -1)))

(defun glf-sync-infoline ()
  "Move to infoline of current record"
  (beginning-of-line)
  (when (not (looking-at glf-infoline-pattern))
    (if (looking-at glf-location-pattern)
        (forward-line 1)
      (while (and (not (bobp))
                  (not (looking-at glf-infoline-pattern)))
        (forward-line -1)))))

(defun glf-forward-paragraph ()
  "Move to end of thread paragraph."
  ;; in other words: go to first line of next paragraph
  (interactive)
  (glf-sync-infoline)
  (let ((tid (glf-read-column glf-thread-index)))
    (while (and (not (eobp))
                (equal tid (glf-read-column glf-thread-index)))
      (glf-forward-infoline))
    (when (interactive-p)
      (message "Reached end of paragraph %s" tid))))

(defun glf-backward-paragraph ()
  "Move backward to start of thread paragraph."
  ;; go to first line of current paragraph
  (interactive)
  (glf-backward-infoline)
  (let ((tid (glf-read-column glf-thread-index)))
    (while (and (not (bobp))
                (equal tid (glf-read-column glf-thread-index)))
      (glf-backward-infoline))
    (glf-forward-infoline)
    (when (interactive-p)
      (message "Reached beginning of paragraph %s" tid))))

(defun glf-beginning-of-paragraph ()
  (glf-sync-infoline)
  (let ((tid (glf-read-column glf-thread-index))
        (previous (save-excursion (glf-backward-infoline) (glf-read-column glf-thread-index))))
    (when (equal tid previous)
      (glf-backward-paragraph))))

(defun glf-end-of-paragraph ()
  (glf-forward-paragraph)
  (when (not (eobp))
    (forward-line -1)
    (unless (looking-at glf-location-pattern)
      (forward-line 1))))

(defun glf-forward-thread ()
  "Go to the next line of same thread"
  (interactive)
  (glf-sync-infoline)
  (let ((origin (point))
        (tid (glf-read-column glf-thread-index)))

    (while (progn
             (glf-forward-infoline)
             (and (not (eobp))
                  (not (equal tid (glf-read-column glf-thread-index))))))

    (unless (equal tid (glf-read-column glf-thread-index))
      (message "Thread %s ends here" tid)
      (goto-char origin))))

(defun glf-backward-thread ()
  "Go to the previous line of same thread"
  (interactive)
  (glf-sync-infoline)
  (let ((origin (point))
        (tid (glf-read-column glf-thread-index)))

    (while (progn
             (glf-backward-infoline)
             (and (not (bobp))
                  (not (equal tid (glf-read-column glf-thread-index))))))

    (unless (equal tid (glf-read-column glf-thread-index))
      (message "Thread %s starts here" tid)
      (goto-char origin))))

;;;
;;; Indentation
;;;

(defvar glf-indent-width 3 "Indentation size")

(defun glf-find-overlays-specifying (prop)
  (let ((result))
    (dolist (ov (overlays-at (point)) result)
      (if (overlay-get ov prop)
          (setq result (cons ov result))))
    result))

(defun glf-read-depth ()
  "Read depth of current log line"
  (let
      ((depth (string-to-number (glf-read-column (glf-get-index "MinorDepth"))))
       (scope (glf-read-column (glf-get-index "ScopeTag"))))
      (cond
       ((equal scope "{")  (1- depth))
       ((equal scope "}")  (1- depth))
       (t                  depth))))

(defun glf-search-indenter ()
  "Move to indenter overlay of current line and read it"

  (glf-goto-field (glf-get-index "Text"))

  (let ((indenters (glf-find-overlays-specifying  'glf-indent)))
    (if (null indenters)
        nil
      (car indenters))))

(defun glf-update-indenter (overlay size)
  "Modify overlay so that it becomes an indenter of the given size"
  (overlay-put overlay 'glf-indent size)
  (overlay-put overlay 'priority 0)
  (overlay-put overlay 'before-string (make-string (* (max 0 size) glf-indent-width) ?\ )))

(defun glf-indent-line ()
  "Indent current line"
  (interactive)
  (when (save-excursion
          (beginning-of-line)
          (looking-at glf-infoline-pattern))
    (let ((overlay  (glf-search-indenter)))
      (when (null overlay)
        (setq overlay (make-overlay (point) (1+ (point)))))
      (glf-update-indenter overlay (glf-read-depth)))))

(defun glf-indent-paragraph ()
  "Indent current current paragraph"
  (interactive)
  (glf-sync-infoline)
  (save-excursion
    (glf-indent-region
     (progn (glf-beginning-of-paragraph) (point))
     (progn (glf-end-of-paragraph) (point)))))

(defun glf-indent-region (beg end)
  "Indent region"
  (interactive "r")

  (overlay-recenter end)

  (save-excursion
    (goto-char beg)

    (let ((countLines (count-lines beg end))
         (line 0))
      (while (< (point) end)
        (setq line (1+ line))
        (when (zerop (% line 50))
          (message (format "Indenting line %d/%d" line countLines)))

        (glf-indent-line)
        (forward-line 1))
      (message "Indenting done: %d lines" countLines))))

(defun glf-find-regions (end-function &optional start-function)
  "make a list of pairs of region's start and end positions"
  (let ((regions nil))
    (while (not (eobp))
      (let ((start-point (point)))
        (funcall end-function)
        (push (cons start-point (point)) regions)
        (when start-function (funcall start-function))))
    regions))

;;;
;;; Information summary
;;;

(defun glf-hashmap-keys (map)
  "Returns the map keys"
  (let ((result))
    (maphash (lambda (key val) (setq result (cons key result))) map)
    (sort result 'string<)))

(defun glf-errors-summary ()
  "Make a summary of errors"
  (interactive)
  (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "^HEADER_END" nil t)
        (error "Invalid GLF file, no header found."))

      (let ((error-map (make-hash-table :test 'equal))
            (exception-map (make-hash-table :test 'equal))
            (assert-map (make-hash-table :test 'equal))
            (error-regexp (format "%c\\([AE]\\)%c\\([ X]\\)%c.*%c\\([^%c\r\n]+\\)"
                                  glf-column-separator glf-column-separator
                                  glf-column-separator glf-column-separator
                                  glf-column-separator)))
        (while (re-search-forward error-regexp nil t)
          (let ((error-type (string-to-char (match-string-no-properties 1)))
                (exception-p (string= (match-string-no-properties 2) "X"))
                (error-message
                 (replace-regexp-in-string "\\<[1-9][0-9]+\\>" "<integer>"
                                           (replace-regexp-in-string "\\<[0-9]+[.][0-9]+\\>" "<float>"
                                                                     (match-string-no-properties 3)))))
            (cond (exception-p (puthash error-message t exception-map))
                  ((eq error-type ?A) (puthash error-message t assert-map))
                  ((eq error-type ?E) (puthash error-message t error-map)))))
        (if (and (= (hash-table-count assert-map) 0)
                 (= (hash-table-count error-map) 0)
                 (= (hash-table-count exception-map) 0))
            (message "No errors")
          (let ((sections (list (cons "Errors" error-map)
                                (cons "Exceptions" exception-map)
                                (cons "Asserts" assert-map))))
            (switch-to-buffer-other-window (get-buffer-create "*GLF error summary*"))
            (erase-buffer)

            (dolist (section sections)
              (let ((section-name (car section))
                    (messages (glf-hashmap-keys (cdr section))))
                (when (> (length messages) 0)
                  (insert section-name)
                  (newline)
                  (insert (make-string (length section-name) ?-))
                  (newline)
                  (while messages
                    (insert (car messages))
                    (newline)
                    (setq messages (cdr messages)))
                  (newline)))) )))))

;;;
;;; Thread focus
;;;

(defconst glf-invisible-thread-alist
  '((glf-focus . t) (invisible . t) (priority . 5)))

(defun glf-toggle-thread-focus (tid)
  "Display only the given thread or display them all if given a nil parameter"
  (interactive
   (if (null glf-thread-overlays)
       (let ((current (progn (glf-sync-infoline) (glf-read-column glf-thread-index))))
         (list (read-string (format "Focus on thread (default %s): " current) nil nil current)))
     (list nil)))

  (if tid
      (glf-thread-focus tid)
    (glf-thread-unfocus)
    (message "Showing all threads")))

(defun glf-thread-unfocus ()
  "Display all threads"
  (mapc (lambda (overlay) (delete-overlay overlay)) glf-thread-overlays)
  (setq glf-thread-overlays ()))

(defun glf-thread-focus (tid)
  "Display only the given thread"
  (glf-thread-unfocus)

  (save-excursion
    (goto-char glf-end-of-header-point)
    (glf-find-paragraph-not-on-thread tid)

    (setq glf-thread-overlays
	  (mapcar (function (lambda (reg) (glf-overlay-region (car reg) (cdr reg) glf-invisible-thread-alist)))
		  (glf-find-regions 'glf-end-of-paragraph
				    (lambda () (glf-find-paragraph-not-on-thread tid)))))))

(defun glf-find-paragraph-not-on-thread (tid)
  "Find the next paragraph that do not belong to the given thread or stay at current position"
  (while (and (not (eobp))
	      (string= tid (save-excursion (glf-sync-infoline)(glf-read-column glf-thread-index))))
    (glf-end-of-paragraph)))

(defun glf-overlay-region (start end alist)
  (let ((overlay (make-overlay start end)))
    (dolist (property alist overlay)
      (overlay-put overlay (car property) (cdr property)))))

;;;
;;; Collapse expand
;;;

(defconst glf-collapsed-alist
  '((glf-collapse . t) (invisible . glf-collapse) (priority . 2) (isearch-open-invisible . glf-unset-invisible)))

(defun glf-unset-invisible (ov)
  (overlay-put ov 'invisible nil))

(defun glf-read-collapser ()
  "Read a collapser overlay"
  (let ((overlays (glf-find-overlays-specifying  'glf-collapse)))
    (if (null overlays)
        nil
      (car overlays))))

(defun glf-collapse-expand ()
  "Collapse or expand the current paragraph"
  (interactive)
  (save-excursion
    (glf-beginning-of-paragraph)

    (let ((beg (point))
          (end (save-excursion (glf-end-of-paragraph) (point))))
      (glf-collapse-region beg end))))


(defun glf-collapse-region (beg end &optional collapse-p)
  "Collapse or expand the given region"
  (goto-char beg)
  (forward-line)

  (when (not (equal end (point))) ; when region is too small, we ignore it

    (let* ((overlay (glf-read-collapser))
           (collapse-p (or (not (null collapse-p))
                           (not (and overlay
                                     (overlay-get overlay 'invisible))))))

      (if collapse-p
          (if overlay
               (overlay-put overlay 'invisible 'glf-collapse)
            (glf-overlay-region (point) (1- end) glf-collapsed-alist))
        (when overlay
          (delete-overlay overlay))))))

(defun glf-collapse-expand-all ()
  "Collapse/expand all paragraphs"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (glf-forward-infoline)
    (setq glf-collapse-all-p (not glf-collapse-all-p))
    (dolist (region (glf-find-regions 'glf-end-of-paragraph
				      'forward-line))
      (glf-collapse-region (car region) (cdr region) glf-collapse-all-p))))

;;;
;;; Hide / show location
;;;

(defconst glf-invisible-location-alist
  '((glf-location . t) (invisible . t) (priority . 2) (isearch-open-invisible . glf-unset-invisible)))

(defun glf-toggle-location-visibility ()
  "Show or hide location lines"
  (interactive)
  (cond
   ((null glf-location-overlays)
    (setq glf-location-overlays (glf-make-hiding-overlays)
	  glf-location-visible-p nil))
   (glf-location-visible-p
    (mapc (lambda (overlay) (overlay-put overlay 'invisible t)) glf-location-overlays)
    (setq glf-location-visible-p nil))
   (t
    (mapc (lambda (overlay) (overlay-put overlay 'invisible nil)) glf-location-overlays)
    (setq glf-location-visible-p t))))

(defun glf-make-hiding-overlays ()
  (save-excursion
    (goto-char glf-end-of-header-point)
    (glf-find-location-line)

    (mapcar (function (lambda (reg) (glf-overlay-region (car reg) (cdr reg) glf-invisible-location-alist)))
	    (glf-find-regions 'forward-line
			      'glf-find-location-line))))

(defun glf-find-location-line ()
  (while (and (not (eobp))
	      (not (looking-at-p glf-location-pattern)))
    (forward-line 1)))

;;;
;;; Visit referenced files: xml trace and code location
;;;

(defun glf-jit-process (beg end)
  (goto-char beg)
  (while
      (let ((lep (line-end-position)))

        (glf-link-trace-file beg lep)
	(glf-link-location beg lep)

        (let ((next (1+ lep)))
          (if (< next end)
              (goto-char next)
            nil)))))

(defun glf-link-trace-file (beg end)
  "Define clickable text on XML trace file"

  (while (re-search-forward "TraceFile_Name:\\(.*\\.xml\\)" end t)
    (let*
        ((mbp (match-beginning 0))
         (path (match-string 1))
         (validPath (glf-validate-trace-path path)))

      (when validPath
	(let ((map (make-sparse-keymap)))
	  (define-key map [mouse-2]        'glf-mouse-find-linked-file)
	  (define-key map [follow-link]    'mouse-face) ;mouse-1 follows link
	  (define-key map (kbd "RET")      'glf-find-linked-file)

	  (add-text-properties mbp (point)
			       `(mouse-face highlight keymap ,map
					    help-echo "mouse-1 / RET: visit this file in other window"
					    glf-linked-trace-file ,validPath)) )))))

(defun glf-validate-trace-path (str)
  "Transform the input path into a valid one (if possible)"
  (if (file-exists-p str)
      str
    ;; many paths are malformed and must be fixed
    (let* ((pattern "wicdztrace")
           (index  (string-match pattern str)))
      (if (null index)
          nil
        (setq str (substring str (+ 1 index (length pattern))))
        (setq str (concat (file-name-as-directory pattern) str))
	str))))

(defun glf-mouse-find-linked-file (event)
  "Visit the file or directory name you click on."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))

    (when (not (windowp window))
        (error "No file chosen"))

    (with-current-buffer (window-buffer window)
      (select-window window)
      (glf-find-linked-file pos))))

(defun glf-find-linked-file (&optional position)
  "Visit a file or a directory"
  (interactive)
  (let
      ((file (get-text-property (or position (point)) 'glf-linked-trace-file)))
    (cond
     ((null file)             (message "No file at this position"))
     ((file-directory-p file) (dired-other-window file))
     ((file-regular-p file)   (find-file-other-window file))
     (t                       (message "File not found: %s" file)))))

(defun glf-link-location (beg end)
  "Define clickable text on location"
  (while (re-search-forward glf-location-pattern end t)
    (let*
        ((mbp (match-beginning 0))
         (path (match-string 1)))

      (let ((map (make-sparse-keymap)))
	(define-key map [mouse-2]        'glf-mouse-find-linked-location)
	(define-key map [follow-link]    'mouse-face) ;mouse-1 follows link
	(define-key map (kbd "RET")      'glf-find-linked-location)

      (add-text-properties mbp (point)
			   `(mouse-face highlight keymap ,map
					help-echo "mouse-1 / RET: jump to this buffer and line")) ))))

(defun glf-mouse-find-linked-location (event)
  "Visit the location you click on."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))

    (when (not (windowp window))
        (error "No link to follow"))

    (with-current-buffer (window-buffer window)
      (select-window window)
      (glf-find-linked-location))))

(defun glf-find-linked-location ()
  (interactive)
  (beginning-of-line)
  (glf-visit-location-at-point))

(defun glf-find-source-file ()
  "Goto file and line that correspond to the current message"
  (interactive)
  (save-excursion
    (glf-sync-infoline)
    (forward-line -1)
    (glf-visit-location-at-point)))

(defun glf-visit-location-at-point ()
  (unless (looking-at glf-location-pattern)
    (error "No location found"))

  (let ((pathfile (match-string-no-properties 1))
	(lineno (string-to-number (match-string-no-properties 2))))
    (let* ((pathparts (split-string pathfile "[\\/\\\\]"))
	   (filename (car (last pathparts)))
	   (buffer (get-buffer filename)))

      (if buffer
	  (progn
	    (switch-to-buffer-other-window buffer)
	    (goto-char (point-min))
	    (if (> lineno 1)
		(forward-line (- lineno 1)))
	    (beginning-of-line))
	(if (y-or-n-p (format "%s: buffer not found. Run file search? " filename))
	    (find-name-dired (read-directory-name "Search in directory: ")
			     filename))))))

;;;
;;; keymap
;;;

(defvar glf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-next>")    'glf-next-error)
    (define-key map (kbd "<C-prior>")   'glf-previous-error)

    (define-key map (kbd "C-c RET")     'glf-find-source-file)

    (define-key map (kbd "<C-down>")    'glf-forward-paragraph)
    (define-key map (kbd "<C-up>")      'glf-backward-paragraph)

    (define-key map (kbd "<M-down>")    'glf-forward-thread)
    (define-key map (kbd "<M-up>")      'glf-backward-thread)

    (define-key map (kbd "C-c C-f")     'glf-toggle-thread-focus)

    (define-key map (kbd "C-c C-c")     'glf-collapse-expand)
    (define-key map (kbd "C-c C-a")     'glf-collapse-expand-all)

    (define-key map (kbd "C-c S")       'glf-errors-summary)

    (define-key map (kbd "C-c C-l")     'glf-toggle-location-visibility)
    map)
  "Keymap for `glf-mode' mode")


(defun glf-regexp-quote-char (c)
  (regexp-quote (char-to-string c)))

;;;###autoload
(define-derived-mode glf-mode fundamental-mode "glf"
  "Major mode for viewing GLF files."
  (glf-parse-header)

  (use-local-map glf-mode-map)

  ;; pre-compute indexes that are used a lot
  (set (make-local-variable 'glf-thread-index) (glf-get-index "ThreadID"))
  ;; patterns
  ;;(setq glf-column-pattern (format "|\\(?:.\\|[^|\n]\\)*"
  (set (make-local-variable 'glf-column-pattern) (format "%s\\(?:%s.\\|[^%s\n]\\)*"
							 (glf-regexp-quote-char glf-column-separator)
							 (glf-regexp-quote-char glf-escape-char)
							 (glf-regexp-quote-char glf-column-separator)))
  (set (make-local-variable 'glf-location-pattern) (format "^\\([^%c][^:\r\n]+\\):\\([[:digit:]]+\\):" glf-column-separator))
  (set (make-local-variable 'glf-infoline-pattern) (format "^%c[a-f0-9\-]+%c" glf-column-separator glf-column-separator))
  ;; font-lock
  (set (make-local-variable 'glf-next-background-color) 0)
  (set (make-local-variable 'glf-thread-faces-map) (make-hash-table :test 'equal))
  (set (make-local-variable 'glf-font-lock-column-specification) (glf-compute-font-lock-column-specification))
  (set (make-local-variable 'glf-thread-match-data-index) (glf-compute-thread-match-data-index))
  (set (make-local-variable 'font-lock-defaults) '(glf-font-lock-keywords t))
  ;; indenting
  (set (make-local-variable 'indent-line-function) 'glf-indent-paragraph)
  (set (make-local-variable 'indent-region-function) 'glf-indent-region)
  ;; thread focus
  (set (make-local-variable 'glf-thread-overlays) nil)
  ;; collapse expand
  (set (make-local-variable 'glf-collapse-all-p) nil)
  (add-to-invisibility-spec '(glf-collapse . t)) ;display as ellipsis
  ;; location visibility
  (set (make-local-variable 'glf-location-overlays) nil)
  (set (make-local-variable 'glf-location-visible-p) t)

  (when (fboundp 'jit-lock-register)
    (jit-lock-register 'glf-jit-process)))


;;; provide myself
(provide 'glf-mode)
