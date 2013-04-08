;;;
;;; Faces
;;;

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

;; try ielm ???
;; try font-lock-refresh-defaults

(defconst glf-background-colors
  '("cornsilk"
    "lavender"
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
    "rosi brown"
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
				(if (eq font :invisible) invisible-face font));??? insert reference???
			  res))
	  (setq index (1+ index)))))))

;;?? for debug (setq font-lock-support-mode nil)

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

(setq titi (format "[%c%c]" glf-column-separator glf-record-separator));????

  (let
      ((begin (if isInvisible (1- (point)) (point)))
       (end (re-search-forward titi search-limit t count)))
    (unless end (throw 'badmatch nil))
    (list begin (1- end))))

(defun skip-column (search-limit count)
(setq titi (format "[%c%c]" glf-column-separator glf-record-separator));????
  (when (not (re-search-forward titi search-limit t count))
    (throw 'badmatch nil)))

(defun glf-font-lock-keywords ()
  (list
   (cons 'glf-column-matcher (glf-compute-font-lock-column-faces)) ;??? variable ou pas?
   )
;  "Build font lock keywords for `glf-mode'."
)

;  (let ((sep glf-column-separator))
;    (list
     ;; Location
;?     (if (eq glf-default-location-visibility-mode 'grayed-out)
;?	 (cons (format "^[^%c][^:\r\n]*:[^:\r\n]*:.*\r?" glf-column-separator) 'glf-light-text-face)
;?       (cons glf-location-pattern '((1 glf-filename-face) (2 glf-line-number-face))))


     ;; Columns
;?     (glf-font-lock-columns-keyword '(("Location" . nil)
;?                                      ("Time" . glf-column-time-face)
;?                                      ("Importance" . nil)
;?                                      ("Severity" . nil)
;?                                      ("Exception" . nil)
;?                                      ("ProcessID" . glf-column-process-id-face)
;?                                      ("ThreadID" . glf-column-thread-id-face)
;?                                      ("ScopeTag" . glf-column-scope-tag-face)
;?                                      ("Text" . (glf-get-current-thread-face)))
;?                                    "ThreadID")

     ;; Errors and exceptions
;?     (cons (format "%c\\([AEX]\\)%c.*%c\\([^%c\r\n]*\\)$" sep sep sep sep) '((1 glf-errors-face) (2 glf-errors-face append)))

     ;; Warnings
;?     (cons (format "%c\\(W\\)%c.*%c\\([^%c\r\n]*\\)$" sep sep sep sep) '((1 glf-warnings-face) (2 glf-warnings-face append)))
;     )))


;;;
;;; Parsing file header
;;;
(defun glf-read-header-alist ()
  (let ((header-alist ()))
    (goto-char (point-min))
    (when (looking-at-p "FILE_TYPE:")
      (while (not (looking-at-p "HEADER_END"))
	(if (looking-at "\\([A-Z_]+\\):\\(.*\\)")
	    (let ((name (match-string-no-properties 1))
		  (value (match-string-no-properties 2)))
	      (setq header-alist (cons (cons name value) header-alist))
	      (forward-line) ))))
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

      (set (make-local-variable 'glf-end-of-header-point)
	   (if header
	       (progn (forward-line)) (point)
	       (point-min)))

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
    (princ (format "glf-font-lock-column-faces : %s\n" glf-font-lock-column-faces))
    (princ (format "glf-thread-match-data-index : %d\n" glf-thread-match-data-index))
))


;;;###autoload
(define-derived-mode glf-mode fundamental-mode "glf"
  "Major mode for viewing GLF files."
  (glf-parse-header)

  ;; font-lock
  (set (make-local-variable 'glf-next-background-color) 0)
  (set (make-local-variable 'glf-thread-faces-map) (make-hash-table :test 'equal))
  (set (make-local-variable 'glf-font-lock-column-specification) (glf-compute-font-lock-column-specification))
  (set (make-local-variable 'glf-thread-match-data-index) (glf-compute-thread-match-data-index))
  (set (make-local-variable 'glf-font-lock-column-faces) (glf-compute-font-lock-column-faces))
  (set (make-local-variable 'font-lock-defaults) '(glf-font-lock-keywords t))


;?  (set (make-local-variable 'glf-location-visibility-mode) glf-default-location-visibility-mode)

  (set (make-local-variable 'glf-location-pattern) (format "^\\([^%c][^:\r\n]*\\):\\([[:digit:]]+\\):" glf-column-separator))





  (glf-test)
)

(add-to-list 'auto-mode-alist '("\\.glf$" . glf-mode))

;;; provide myself
(provide 'glf-mode)
