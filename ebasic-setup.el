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

; FIXME: arrays should have arbitrary dimensions

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
  (if (listp value)
      (setq value (ebasic-eval value)))
  (if (eq (ebasic-stringvarp varname) (stringp value))
      (if (member varname ebasic-vars)
          (if index
              (if (listp index)
                  (aset (eval (ebasic-var-to-symbol varname))
                        (ebasic-eval index) value)
                (aset (eval (ebasic-var-to-symbol varname)) index value))
            (set (ebasic-var-to-symbol varname) value))
        (add-to-list 'ebasic-vars varname)
        (ebasic-set-var varname value index))
    (ebasic-error "Type mismatch error.")))

(defun ebasic-make-string (my-string)
  "Add 'ebasic-string property to MY-STRING to make it an
official ebasic string."
  (propertize my-string 'ebasic-string t))

; FIXME: only strings should be strings, other things should be symbols?

(defun ebasic-eval (expression)
  "Evaluate parsed EXPRESSION, return string or number result."
  (if (not (listp expression))
      (cond
       ((numberp expression)
        expression)
       ((get-text-property 0 'ebasic-string expression)
        expression)
       ((string-match "^\"\\([^\"]*\\)\"$" expression)
        (ebasic-make-string (match-string 1 expression)))
       ((string-match "^\\(-?[.[:digit:]]+\\)$" expression)
        (string-to-number (match-string 1 expression)))
       ((member expression ebasic-vars)
        (ebasic-get-var expression))
       (t
        ; this should never happen
        (read (concat "ebasic-unevaluated/" expression))))
    (cond
     ; list with 1 item: just evaluate it
     ((= (length expression) 1)
      (ebasic-eval (car expression)))
     ; first item is a function, second must be its parameters
     ((and (stringp (car expression))
           (fboundp (read (concat "ebasic/" (car expression)))))
      (ebasic-eval
       (append (list
                (funcall (read (concat "ebasic/" (car expression)))
                         (ebasic-eval (cadr expression))))
               (cddr expression))))
     ; second item is an operator, first and third must be its parameters
     ((and (stringp (cadr expression))
           (fboundp (read (concat "ebasic-operators/" (cadr expression)))))
      (funcall (read (concat "ebasic-operators/" (cadr expression)))
               (ebasic-eval (car expression))
               (ebasic-eval (cddr expression))))
     ; first item is an array variable, second must be its index
     ((and (member (car expression) ebasic-vars)
           (vectorp (ebasic-get-var (car expression))))
      (if (cddr expression)
          (ebasic-eval (append
                        (list
                         (ebasic-get-var (car expression)
                                         (ebasic-eval (cadr expression))))
                        (cddr expression)))
        (ebasic-get-var (car expression)
                        (ebasic-eval (cadr expression)))))
    ; something before this or here should catch and eval functions
     (t
      expression))))
;      (mapcar 'ebasic-eval expression)))))

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
        (var-assign (ebasic-var-to-symbol (car parsed-line)))
        (args (cdr parsed-line)))
    (cond
     ; the first word is a statement
     ((fboundp command)
      (funcall command args))
     ; the second word is "=", meaning variable assignment
     ((and (stringp (car args))
           (string= (car args) "="))
      (ebasic-set-var (car parsed-line) (cdr args)))
     ; the second word is a list and the third word is "=", meaning
     ; array variable assignment
     ((and (listp (car args))
           (stringp (cadr args))
           (string= (cadr args) "="))
      (ebasic-set-var (car parsed-line) (cddr args) (car args)))
     (t
      (ebasic-error "Syntax error.")))))

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
