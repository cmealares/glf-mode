;;; glf-mode.el -- major mode for viewing GLF log files
;;
;;; Authors:
;;     Christophe Mealares
;;     Laurent P
;;
;;; Main features:
;;    Syntax coloration
;;    Focus on a particular thread by hiding others
;;    Thread navigation:
;;      - move to next/previous thread paragraph;
;;      - move to next/previous paragraph in same thread.
;;    Collapse/expand paragraph
;;    Indentation of nested scopes
;;    Errors are indexed by IMenu: setup with (global-set-key [mouse-3] 'imenu)
;;
;;; Installation:
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

(defcustom glf-default-location-visibility-mode 'grayed-out
  "Non nil means to show file location by default."
  :type '(choice (const :tag "Invisible (slow)" :value invisible)
                 (const :tag "Visible" :value visible)
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
  '("cornsilk"
    "misty rose"
    "lavender"
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
    "pink")
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
            ;;(invisible-face 'glf-light-text-face)
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
      (let ((matchlist (list search-limit (point))))
        (dolist (spec speclist)
          (let* ((count (car spec))
                 (face (cdr spec))
                 (match (if (null face)
                            (skip-column search-limit count)
                          (search-column search-limit count (eq face :invisible)))))
            (when match
              (setq matchlist (cons (cadr match) (cons (car match) matchlist))))))
        (nreverse matchlist)))))

(defun search-column (search-limit count isInvisible)
  (let
      ((begin (if isInvisible (1- (point)) (point)))
       (end (re-search-forward glf-end-column-pattern search-limit t count)))
    (unless end (throw 'badmatch nil))
    (list begin (1- end))))

(defun skip-column (search-limit count)
  (unless (re-search-forward glf-end-column-pattern search-limit t count)
    (throw 'badmatch nil)))

(defun glf-font-lock-keywords ()
  "Build font lock keywords for `glf-mode'."
  (let ((sep glf-column-separator))
    (list

     ;; Location
     (if (eq glf-default-location-visibility-mode 'grayed-out)
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
    (unless (looking-at-p "FILE_TYPE:")
      (error "header not found, invalid GLF file"))
    (while (not (looking-at-p "HEADER_END"))
      (if (looking-at "\\([A-Z_]+\\):\\(.*\\)")
          (let ((name (match-string-no-properties 1))
                (value (match-string-no-properties 2)))
            (setq header-alist (cons (cons name value) header-alist))
            (forward-line) )))
    (forward-line) ;;skip HEADER_END
    header-alist))

(defun glf-get-header-value (name type alist default &optional column-separator)
  (let* ((def (assoc name alist))
         (strval  (when def (cdr def))))
    (cond
     ((eq type :string) (if (null strval) default strval))
     ((eq type :char)  (if (null strval) default (string-to-number strval)))
     ((and (eq type :list) column-separator) (if (null strval) default (split-string strval (char-to-string column-separator))))
     (t (error "%s: invalid field type for field %s" (symbol-name type) name)))))

(defun glf-parse-header ()
  (save-excursion
    (let ((header (glf-read-header-alist)))

      (set (make-local-variable 'glf-end-of-header-point) (point))
      (set (make-local-variable 'glf-file-type) (glf-get-header-value "FILE_TYPE" :string header nil))
      (set (make-local-variable 'glf-file-encoding) (glf-get-header-value "ENCODING" :string header "UTF-8"))
      (set (make-local-variable 'glf-record-separator) (glf-get-header-value "RECORD_SEPARATOR" :char header 30))
      (set (make-local-variable 'glf-column-separator) (glf-get-header-value "COLUMN_SEPARATOR" :char header 124))
      (set (make-local-variable 'glf-escape-char) (glf-get-header-value "ESC_CHARACTER" :char header 27))
      ;; ignore location: not a true column
      (set (make-local-variable 'glf-columns) (cdr (glf-get-header-value "COLUMNS" :list header '() glf-column-separator)))

      (let ((nb-columns (length glf-columns)))
        (set (make-local-variable 'glf-column-indexes-map) (make-hash-table :test 'equal :weakness t :size nb-columns))
        (dotimes (i nb-columns)
          (puthash (nth i glf-columns) i glf-column-indexes-map))) )))

(defun glf-test ()
  "Print the result of some reading functions"
  (interactive)
  (with-output-to-temp-buffer "*glf-test*"

    (print "*** Variables")
    (princ (format "glf-file-type : %s" glf-file-type))
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

(defsubst glf-goto-field (n)
  "Move to nth field of the current infoline. Counting starts at 0."
  (beginning-of-line)
  (if (zerop n)
      t
    (search-forward (format "%c" glf-column-separator) (line-end-position) t (1+ n))))

(defun glf-read-column (name)
  "Read column value on current column line. Caller must set position on a column line."
  (save-excursion
    (beginning-of-line)

    (let ((index (gethash name glf-column-indexes-map))
          (separator (format "%c" glf-column-separator)))
      (if (null index)
          (error "Unknown column name: %s" name)

        (let
            ((start-pos (search-forward separator (line-end-position) t (1+ index)))
             (end-pos (search-forward separator (line-end-position) t 1)))
          (if (null start-pos)
              nil
            (buffer-substring-no-properties start-pos
                                            (1- (if (null end-pos) (line-end-position) end-pos)))) )))))

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

(defun glf-goto-file ()
  "Goto file and line that correspond to the current message"
  (interactive)
  (save-excursion
    (glf-sync-infoline)
    (forward-line -1)
    (save-match-data
      (if (looking-at glf-location-pattern)
          (let ((pathfile (match-string-no-properties 1))
                (lineno (string-to-number (match-string-no-properties 2))))
            (let* ((pathparts (split-string pathfile "[\\/\\\\]"))
                   (filename (car (last pathparts)))
                   (buffer (get-buffer filename)))
              (if buffer
                  (progn (switch-to-buffer-other-window buffer)
                         (goto-char (point-min))
                         (if (> lineno 1)
                             (forward-line (- lineno 1)))
                         (beginning-of-line))
                (error "No buffer visiting %s" filename))))
        (error "No location found on this line")))))

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

;;??glf-end-of-record
(defun glf-end-of-record ()
  "Goto the end of current record"
  (interactive)
  (end-of-line)
  (while (and (not (eq (char-before) glf-record-separator)) (not (eq (point) (point-max))))
    (forward-char)
    (end-of-line)))

(defun glf-forward-paragraph ()
  "Move to end of thread paragraph."
  ;; in other words: go to first line of next paragraph
  (interactive)
  (glf-sync-infoline)
  (let ((tid (glf-read-column "ThreadID")))
    (while (and (not (eobp))
                (equal tid (glf-read-column "ThreadID")))
      (glf-forward-infoline))
    (when (interactive-p)
      (message "Reached end of paragraph %s" tid))))

(defun glf-backward-paragraph ()
  "Move backward to start of thread paragraph."
  ;; go to first line of current paragraph
  (interactive)
  (glf-backward-infoline)
  (let ((tid (glf-read-column "ThreadID")))
    (while (and (not (bobp))
                (equal tid (glf-read-column "ThreadID")))
      (glf-backward-infoline))
    (glf-forward-infoline)
    (when (interactive-p)
      (message "Reached beginning of paragraph %s" tid))))

(defun glf-beginning-of-paragraph ()
  (glf-sync-infoline)
  (let ((tid (glf-read-column "ThreadID"))
        (previous (save-excursion (glf-backward-infoline) (glf-read-column "ThreadID"))))
    (when (equal tid previous)
      (glf-backward-paragraph))))

(defun glf-end-of-paragraph ()
  (glf-sync-infoline)
  (let ((tid (glf-read-column "ThreadID"))
        (next (save-excursion (glf-forward-infoline) (glf-read-column "ThreadID"))))
    (if (equal tid next)
      (glf-forward-paragraph)
      (forward-line 1))))

(defun glf-forward-thread ()
  "Go to the next line of same thread"
  (interactive)
  (glf-sync-infoline)
  (let ((origin (point))
        (tid (glf-read-column "ThreadID")))

    (while (progn
             (glf-forward-infoline)
             (and (not (eobp))
                  (not (equal tid (glf-read-column "ThreadID"))))))

    (unless (equal tid (glf-read-column "ThreadID"))
      (message "Thread %s ends here" tid)
      (goto-char origin))))

(defun glf-backward-thread ()
  "Go to the previous line of same thread"
  (interactive)
  (glf-sync-infoline)
  (let ((origin (point))
        (tid (glf-read-column "ThreadID")))

    (while (progn
             (glf-backward-infoline)
             (and (not (bobp))
                  (not (equal tid (glf-read-column "ThreadID"))))))

    (unless (equal tid (glf-read-column "ThreadID"))
      (message "Thread %s starts here" tid)
      (goto-char origin))))

;;;
;;; Indentation
;;;
(defvar glf-indent-width 3 "Indentation size")

(defsubst glf-find-overlays-specifying (prop)
  (let ((result))
    (dolist (ov (overlays-at (point)) result)
      (if (overlay-get ov prop)
          (setq result (cons ov result))))
    result))

(defsubst glf-read-depth ()
  "Read depth of current log line"
  (let
      ((depth (string-to-number (glf-read-column "MinorDepth")))
       (scope (glf-read-column "ScopeTag")))
      (cond
       ((equal scope "{")  (1- depth))
       ((equal scope "}")  (1- depth))
       (t                  depth))))

(defsubst glf-search-indenter ()
  "Move to indenter overlay of current line and read it"

  (let ((index (gethash "Text" glf-column-indexes-map)))
    (glf-goto-field index))

  (let ((indenters (glf-find-overlays-specifying  'glf-indent)))
    (if (null indenters)
        nil
      (car indenters))))

(defun glf-update-indenter (overlay size)
  "Modify overlay so that it becomes an indenter of the given size"
  (overlay-put overlay 'glf-indent size)
  (overlay-put overlay 'priority 0)
  (overlay-put overlay 'before-string (make-string (* (max 0 size) glf-indent-width) ?\ )))

(defsubst glf-indent-line ()
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

(defun glf-thread-unfocus ()
  "Display all threads"
  (mapc (lambda (overlay) (delete-overlay overlay)) glf-invisible-overlays)
  (setq glf-invisible-overlays ()))

(defun glf-thread-focus (tid)
  "Display only the given thread"

  (glf-thread-unfocus)

  (setq glf-invisible-overlays
	(glf-overlay-regions
	 (function (lambda()
		     (save-excursion
		       (glf-sync-infoline)
		       (not (string= (glf-read-column "ThreadID") tid)))))
	 (function (lambda ()
		     (glf-forward-paragraph)
		     (when (not (eobp))
		       (forward-line -1)
		       (unless (looking-at glf-location-pattern)
			 (forward-line 1)))))
	 glf-invisible-thread-alist)))

(defun glf-toggle-thread-focus (tid)
  "Display only the given thread or display them all if given a nil parameter"
  (interactive
   (if (null glf-invisible-overlays)
       (let ((current (progn (glf-sync-infoline) (glf-read-column "ThreadID"))))
	 (list (read-string "Focus on thread: " current)))
     (list nil)))

  (if tid
      (glf-thread-focus tid)
    (glf-thread-unfocus)
    (message "Showing all threads")))

(defun glf-overlay-regions (apply-overlay-p next-region alist)
  "Overlay regions that match the given predicate"
  (save-excursion
    (overlay-recenter (point-max))

    (let ((overlays '())
	  (start-pos (goto-char glf-end-of-header-point)))

      (while (not (eobp))
	(let ((apply-p (funcall apply-overlay-p)))
	  (funcall next-region)
	  (when apply-p
	    (setq overlays (cons (glf-overlay-region start-pos (point) alist)
				 overlays)))
	  (setq start-pos (point)) ))
      overlays)))

(defun glf-overlay-region (start end alist)
  (let ((overlay (make-overlay start end)))
    (dolist (property alist overlay)
      (overlay-put overlay (car property) (cdr property)))))

;;;
;;; Open xml trace file with mouse
;;;

(defun glf-jit-process (beg end)
  (interactive "r")
  (goto-char beg)
  (while
      (let ((lbp (line-beginning-position))
            (lep (line-end-position)))

        (glf-link-file beg lep)
        (let ((next (1+ lep)))
          (if (< next end)
              (goto-char next)
            nil)))))

(defun glf-link-file (beg end)
  "Define clickable text on XML trace file"

  (while (search-forward-regexp
          ; ?? could font lock for this pattern ?
          ; ?? location should be clickable too
          "TraceFile_Name:\\(.*\\.xml\\)"
          ;"\\(?:\\.\\.\\|[a-zA-Z]:\\)?\\([\\/][- ~._()a-z0-9A-Z]*\\)+[\\/]"
          end t)
    (let*
        ((mbp (match-beginning 0))
         (path (match-string 1))
         (validPath (glf-validate-path path)))

      (if validPath
          (add-text-properties
           mbp
           (point)
           `(mouse-face highlight
                        help-echo "mouse-2: visit this file in other window"
                        glf-linked-file ,validPath)) ))))

(defun glf-mouse-find-file-other-window (event)
  "Visit the file or directory name you click on."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event)))
        file)
    (when (not (windowp window))
        (error "No file chosen"))
    (with-current-buffer (window-buffer window)
      (setq file (get-text-property pos 'glf-linked-file))

     (cond
      ((null file)             (message "No file at this position"))
      ((file-directory-p file) (select-window window)(dired-other-window file))
      ((file-regular-p file)   (select-window window)(find-file-other-window file))
      (t                       (message "File not found: %s" file))))))

(defun glf-return-find-file-other-window ()
  "Visit a file or a directory"
  (interactive)
  (let
      ((file  (get-text-property (point) 'glf-linked-file)))
      (cond
       ((null file)             (message "No file at this position"))
       ((file-directory-p file) (dired-other-window file))
       ((file-regular-p file)   (find-file-other-window file))
       (t                       (message "File not found: %s" file)))))

(defun glf-validate-path (str)
  "Transform the input path into a valid one (if possible)"
  (if (file-exists-p str)
      str
    ;; often, paths are malformed and must be fixed
    (let* ((pattern "wicdztrace")
           (index  (string-match pattern str)))
      (if (null index)
          nil
        (setq str (substring str (+ 1 index (length pattern))))
        (setq str (concat (file-name-as-directory pattern) str))

        (if (file-exists-p str)
            str
          nil)))))

;;;
;;; keymap
;;;

(defvar glf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-next>")    'glf-next-error)
    (define-key map (kbd "<C-prior>")   'glf-previous-error)
    (define-key map (kbd "C-c RET")     'glf-goto-file)

    ;; open xml trace file
    (define-key map [mouse-2]           'glf-mouse-find-file-other-window)
    (define-key map (kbd "C-c C-v")     'glf-return-find-file-other-window)
    (define-key map [follow-link]       'mouse-face) ;mouse-1 follows link

    (define-key map (kbd "<C-down>")    'glf-forward-paragraph)
    (define-key map (kbd "<C-up>")      'glf-backward-paragraph)

    (define-key map (kbd "<M-down>")    'glf-forward-thread)
    (define-key map (kbd "<M-up>")      'glf-backward-thread)

    (define-key map (kbd "C-c C-f")   'glf-toggle-thread-focus)

    (define-key map (kbd "C-c S")     'glf-errors-summary)
;;    (define-key map (kbd "C-c C-t")   'glf-toggle-truncate-lines)
;;    (define-key map (kbd "C-c C-l")   'glf-toggle-location-visibility)
    map)
  "Keymap for `glf-mode' mode")


;;;###autoload
(define-derived-mode glf-mode fundamental-mode "glf"
  "Major mode for viewing GLF files."
  (glf-parse-header)

  (use-local-map glf-mode-map)

  ;; patterns
  (set (make-local-variable 'glf-end-column-pattern) (format "%c\\|$" glf-column-separator))
  (set (make-local-variable 'glf-location-pattern) (format "^\\([^%c][^:\r\n]+\\):\\([[:digit:]]+\\):" glf-column-separator))
  (set (make-local-variable 'glf-infoline-pattern) (format "^%c[a-f0-9\-]+%c" glf-column-separator glf-column-separator))
  ;; font-lock
  (set (make-local-variable 'glf-next-background-color) 0)
  (set (make-local-variable 'glf-location-visibility-mode) glf-default-location-visibility-mode)
  (set (make-local-variable 'glf-thread-faces-map) (make-hash-table :test 'equal))
  (set (make-local-variable 'glf-font-lock-column-specification) (glf-compute-font-lock-column-specification))
  (set (make-local-variable 'glf-thread-match-data-index) (glf-compute-thread-match-data-index))
  (set (make-local-variable 'font-lock-defaults) '(glf-font-lock-keywords t))
  ;; indenting
  (set (make-local-variable 'indent-line-function) 'glf-indent-paragraph)
  (set (make-local-variable 'indent-region-function) 'glf-indent-region)
  ;; thread focus
  (set (make-local-variable 'glf-invisible-overlays) nil)


;??? TODO manage all 3 cases
;  (if (eq glf-default-location-visibility-mode 'invisible)
;      (glf-toggle-location-visibility))

  (when (fboundp 'jit-lock-register)
    (jit-lock-register 'glf-jit-process)))


;;; provide myself
(provide 'glf-mode)
