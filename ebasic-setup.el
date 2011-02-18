; ebasic-setup.el
; By Charles Sebold

(defvar ebasic-array-re
  "\\([[:alpha:]]+\\(\\$?\\)\\)(\\([[:digit:]]+\\)"
  ; FIXME: the array index will have to be replaced by a parser
  "Regexp to parse variable name, string flag, and index.")

(defun ebasic-var-to-symbol (varname)
  "Convert ebasic variable name VARNAME to emacs symbol."
  (read (concat "ebasic-var/" varname)))

(defun ebasic-error (string &optional linenum)
  "Error message."
  (if linenum
      (message "Error: Line %d: %s" linenum string)
    (message "Error: %s" string)))

(defun ebasic-get-var (varname &optional index)
  "Get variable contents of VARNAME.  If INDEX is non-nil, then
return contents of array VARNAME at INDEX."
  (if (member varname ebasic-vars)
      (if index
          (elt (eval (ebasic-var-to-symbol varname)) index)
        (eval (ebasic-var-to-symbol varname)))
;    (ebasic-error "Variable not defined.")))
    ; I think actually this just returns zeroes and empty strings
    (if index
        (ebasic-error "Variable not defined.")
      (if (ebasic-stringvarp varname) "" 0))))

(defun ebasic-stringvarp (varname)
  "Returns t if VARNAME is a string variable, nil if not."
  (if (string-match "\\$$" varname) t nil))

(defun ebasic-set-var (varname value &optional index)
  "Set variable contents of VARNAME to VALUE.  If INDEX is
non-nil, then set array VARNAME at INDEX to VALUE."
  (if (eq (ebasic-stringvarp varname) (stringp value))
      (if (member varname ebasic-vars)
          (if index
              (aset (eval (ebasic-var-to-symbol varname)) index value)
            (set (ebasic-var-to-symbol varname) value))
        (add-to-list 'ebasic-vars varname)
        (ebasic-set-var varname value index))
    (ebasic-error "Type mismatch error.")))

(defun ebasic-eval (expression)
  "Dummy function for now, FIXME"
  expression)

(defun ebasic-execute (line)
  "Execute command in LINE."
  (let ((my-line (ebasic-split-list (ebasic-parse line) "'" "rem")))
    (if (listp (car my-line))
        (if (= (length my-line) 1)
            (ebasic-execute-parsed (car my-line))
          (mapc 'ebasic-execute-parsed my-line))
      (ebasic-execute-parsed my-line))))

(defun ebasic-execute-parsed (parsed-line)
  "Execute command in list PARSED-LINE."
  (let ((command (read (concat "ebasic/" (car parsed-line))))
        (args (cdr parsed-line)))
    (if (fboundp command)
        (funcall command args)
      (ebasic-error "Syntax error."))))

(defun ebasic-detokenize (return-list stack-hash)
  "Remove tokens stored in STACK-HASH from RETURN-LIST."
  (mapcar (lambda (x)
            (cond
             ((listp x)
              (ebasic-detokenize x stack-hash))
             ((string-match "^\000xe\\([[:digit:]]+\\)\000$" x)
              (ebasic-detokenize (gethash (string-to-number (match-string 1 x)) stack-hash) stack-hash))
             (t x))) return-list))

(defun ebasic-parse (expression &optional stack-hash)
  "Parse EXPRESSION as a BASIC/algebraic expression."
  (let (leave-tokens return-value)
    (unless stack-hash
      (setq stack-hash (make-hash-table :test 'eql))
      (setq leave-tokens t))
    (save-match-data
      (setq return-value
            (cond
             ((and (stringp expression) (string= expression ""))
              nil)
             ((string-match "^\\(.*?\\)\\(\"[^\"]*\"\\)\\(.*\\)$" expression)
              (append (ebasic-parse (match-string 1 expression) stack-hash)
                      (list (match-string 2 expression))
                      (ebasic-parse (match-string 3 expression) stack-hash)))
             ((string-match "^\\(.*\\)(\\([^)]*?\\))\\(.*\\)$" expression)
              (let ((tempvar (ebasic-parse (match-string 2 expression)
                                           stack-hash)))
                (puthash (hash-table-count stack-hash)
                         tempvar stack-hash))
              (ebasic-parse
               (concat (match-string 1 expression)
                       (format "\000xe%d\000" (1- (hash-table-count stack-hash)))
                       (match-string 3 expression)) stack-hash))
             ((string-match "^\\(.*\\)\\(\000xe[[:digit:]]+\000\\)\\(.*\\)$" expression)
              (append (ebasic-parse (match-string 1 expression) stack-hash)
                      (list (match-string 2 expression))
                      (ebasic-parse (match-string 3 expression) stack-hash)))
             ((string-match "^\\(.*?\\)[[:space:]]*\\([-+*/,'^]\\)[[:space:]]*\\(.*\\)$" expression)
              (append (ebasic-parse (match-string 1 expression) stack-hash)
                      (list (match-string 2 expression))
                      (ebasic-parse (match-string 3 expression) stack-hash)))
             ((string-match "^[[:space:]]*\\([^[:space:]]+\\)[[:space:]]*\\(.*\\)$" expression)
              (append (list (match-string 1 expression))
                      (ebasic-parse (match-string 2 expression) stack-hash)))
             (t (list expression)))))
    (if leave-tokens
        (ebasic-detokenize return-value stack-hash)
      return-value)))

(defun ebasic-split-list (in-list &rest delimiter)
  "Split a simple IN-LIST into separate lists based on strings equal to DELIMITER."
  (let (out-list temp-list)
    (while in-list
      (if (and (stringp (car in-list))
               (member (car in-list) delimiter))
          (progn
            (setq out-list (append (list (reverse temp-list)) out-list))
            (setq temp-list nil))
        (setq temp-list (append (list (car in-list)) temp-list)))
      (setq in-list (cdr in-list)))
    (setq out-list (append (list (reverse temp-list)) out-list))
    (reverse out-list)))

(provide 'ebasic-setup)
