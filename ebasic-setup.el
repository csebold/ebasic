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

(defun ebasic-get-var (varname)
  "Get variable contents of VARNAME."
  (if (string-match ebasic-array-re varname)
      (let ((varrealname (match-string 1 varname))
            (index (string-to-number (match-string 3 varname))))
        (if (member varrealname ebasic-vars)
            (elt (eval (ebasic-var-to-symbol varrealname)) index)
          (ebasic-error "Array not dimensioned.")))
    (if (member varname ebasic-vars)
        (eval (ebasic-var-to-symbol varname))
      (ebasic-error "Variable not defined."))))

(defun ebasic-set-var (varname value)
  "Set variable contents of VARNAME to VALUE."
  (if (string-match ebasic-array-re varname)
      (let ((varrealname (match-string 1 varname))
            (index (string-to-number (match-string 3 varname))))
        (if (member varrealname ebasic-vars)
            ; FIXME: need to check to make sure that type is retained
            (aset (eval (ebasic-var-to-symbol varrealname)) index value)
          (ebasic-error "Array not dimensioned.")))
    (if (member varname ebasic-vars)
        (set (ebasic-var-to-symbol varname) value)
      (add-to-list 'ebasic-vars varname)
      ; FIXME: need to check that type is retained
      (set (ebasic-var-to-symbol varname) value))))

(defun ebasic-eval (expression)
  "Dummy function for now, FIXME"
  expression)

(defun ebasic-execute (line)
  "Execute command in LINE; dummy function, FIXME"
  t)

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
             ((string-match "^\\(.*?\\)[[:space:]]*\\([-+*/,^]\\)[[:space:]]*\\(.*\\)$" expression)
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

(defun ebasic-split-list-by-comma (in-list)
  "Split a simple list into separate lists based on comma strings."
  (let (out-list temp-list)
    (while in-list
      (if (and (stringp (car in-list))
               (string= (car in-list) ","))
          (progn
            (setq out-list (append (list (reverse temp-list)) out-list))
            (setq temp-list nil))
        (setq temp-list (append (list (car in-list)) temp-list)))
      (setq in-list (cdr in-list)))
    (setq out-list (append (list (reverse temp-list)) out-list))
    (reverse out-list)))

(provide 'ebasic-setup)
