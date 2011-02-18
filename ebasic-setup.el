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

(defun ebasic-next-parse-block (expression)
  "Find end of first block in EXPRESSION."
  (let ((start-string (substring expression 0 1))
        (search-for
         (cond
          ; text EXPRESSION
          ((string-match "^\"[^\"]*\"" expression)
           (length (match-string 0 expression)))
          ((string-match "^[+-*/^]" expression)
           (length (match-string 0 expression)))
          ((string-match "^[[:digit:]]+" expression)
           (length (match-string 0 expression)))
          (t (string-match "[[:space:]]+" expression 1)))))
    search-for))

; wow, this is hard.

; OK, so if we're looking at an open parenthesis, we need to return the
; open parenthesis, the expression inside, and the closed parenthesis.
; If we find an open parenthesis inside the expression, then we need
; recursively do the same with that.

(defun ebasic-parse (expression)
  "Parse EXPRESSION as a BASIC/algebraic expression."
  (let (exp-list)
    (while (not (string= expression ""))
      (let ((end-block (ebasic-next-parse-block expression)))
        (setq exp-list
              (cons (substring expression 0 end-block) exp-list))
        (setq expression
              (if end-block
                  (substring expression (1+ end-block))
                ""))))
    exp-list))

(defun ebasic-eval (expression)
  "Dummy function for now, FIXME"
  expression)

(defun ebasic-parse (expression)
  "Parse EXPRESSION as a BASIC/algebraic expression."
  (with-temp-buffer
    (let (tokens)
      (insert expression)
      (goto-char (point-min))
      (while (re-search-forward "\".*?\"" nil t)
        (setq tokens (cons (match-string 0) tokens))
        (replace-match (format "\000xb%d\000" (length tokens)))
        (goto-char (point-min)))
      (while (re-search-forward "(\\(.*?\\))" nil t)
        (replace-match (ebasic-eval (match-string 1)))
        (goto-char (point-min)))
;      (while (re-search-forward "[[:alpha:]][[:alnum:]]*(.*?)" nil t)
;        (replace-match (ebasic-eval (match-string 0)))
;        (goto-char (point-min)))
      (while (re-search-forward "\000xb\\([[:digit:]]+\\)\000" nil t)
        (replace-match (nth (string-to-number (string-match 1)) tokens))
        (goto-char (point-min))))
    (buffer-string)))

(defun ebasic-detokenize (return-list stack-hash)
  "Remove tokens stored in STACK-HASH from RETURN-LIST."
  (mapcar (lambda (x)
            (cond
             ((listp x)
              (ebasic-detokenize x stack-hash))
             ((string-match "^\000xe\\([[:digit:]]+\\)\000$" x)
              (gethash (string-to-number (match-string 1 x)) stack-hash))
;              (replace-match (gethash (string-to-number (match-string 1 x)) stack-hash) t t x))
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
             ((string-match "^\\(.*\\)(\\([^)]*?\\))\\(.*\\)$" expression)
              (let ((tempvar (ebasic-parse (match-string 2 expression)
                                           stack-hash)))
                (puthash (hash-table-count stack-hash)
                         tempvar stack-hash))
              (list (ebasic-parse
                     (concat (match-string 1 expression)
                             (format "\000xe%d\000" (1- (hash-table-count stack-hash)))
                             (match-string 3 expression)) stack-hash)))
             ((string-match "^\\(.*\\)\\(\000xe[[:digit:]]+\000\\)\\(.*\\)$" expression)
              (append (ebasic-parse (match-string 1 expression) stack-hash)
                      (list (match-string 2 expression))
                      (ebasic-parse (match-string 3 expression) stack-hash)))
             ((string-match "^\\(.*?\\)\\([-+*/^]\\)\\(.*\\)$" expression)
              (append (ebasic-parse (match-string 1 expression) stack-hash)
                      (list (match-string 2 expression))
                      (ebasic-parse (match-string 3 expression) stack-hash)))
             ((string-match "^\\(.*?\\)\\(\"[^\"]*\"\\)\\(.*\\)$" expression)
              (append (ebasic-parse (match-string 1 expression) stack-hash)
                      (list (match-string 2 expression))
                      (ebasic-parse (match-string 3 expression) stack-hash)))
             ((string-match "^[[:space:]]*\\([^[:space:]]+\\)[[:space:]]*\\(.*\\)" expression)
              (append (list (match-string 1 expression))
                      (ebasic-parse (match-string 2 expression) stack-hash)))
             (t (list expression)))))
    (if leave-tokens
        (ebasic-detokenize return-value stack-hash)
      return-value)))

(provide 'ebasic-setup)
