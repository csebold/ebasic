; ebasic-parse.el

(defvar ebasic-operator-order
  '(exponent times divided plus minus
             equals ne lt le gt ge)
  "Defines operator order of execution in ebasic.")

(defun ebasic-parse-multistatement (inlex)
  "Parse INLEX and return a list of statements."
  (if (not (memq 'colon (mapcar 'car inlex)))
      (list inlex)
    (let (temp this-stmt)
      (dolist (i inlex)
        (if (eq (car i) 'colon)
            (progn
              (push (reverse this-stmt) temp)
              (setq this-stmt nil))
          (push i this-stmt)))
      (push (reverse this-stmt) temp)
      (reverse temp))))

(defun ebasic-parse-group (inlex)
  "Replace lexed parentheses in INLEX with groups of lexed tokens."
  (if (not (memq 'lparen (mapcar 'car inlex)))
      inlex
    (let ((paren-level 0)
          temp group)
      (dolist (i inlex)
        (cond
         ((eq (car i) 'lparen)
          (setq paren-level (1+ paren-level))
          (when (> paren-level 1)
            (push i group)))
         ((eq (car i) 'rparen)
          (when (> paren-level 1)
            (push i group))
          (setq paren-level (1- paren-level))
          (when (= paren-level 0)
            (push (cons 'group (ebasic-parse-group (reverse group))) temp)
            (setq group nil)))
         ((= paren-level 0)
          (push i temp))
         ((eq (car i) 'identifier)
          (push i group))
         (t
          (push i group))))
      (if (or group (not (= paren-level 0)))
          'unmatched-parentheses
        (reverse temp)))))

(defun ebasic-lex-to-sexp (inlex)
  "Convert INLEX to a sexp."
  (if (eq (caar inlex) 'space)
      (ebasic-lex-to-sexp (cdr inlex))
    (cond
     ((or (eq (caar inlex) 'apos)
          (and (eq (caar inlex) 'identifier)
               (string= (cdar inlex) "REM")))
      nil)
     (t
      (let (temp output)
        (dolist (i (cdr inlex))
          (if (listp i)
              (unless (eq (car i) 'space)
                (push i temp))
            (push i temp)))
        (push (read (concat "ebasic/" (cdar inlex))) output)
        (dolist (i (reverse temp))
          (push i output))
        (reverse output))))))

(defun ebasic-eval-constants (token)
  "Convert TOKEN to constant, or return TOKEN."
  (cond
   ((eq (car token) 'group)
    (cons 'group (mapcar 'ebasic-eval-constants (cdr token))))
   ((eq (car token) 'number)
    (string-to-number (cdr token)))
   ((eq (car token) 'string)
    (substring (cdr token) 1 -1))
   (t
    token)))

(defun ebasic-parse (inlex)
  "Run all possible parsing on INLEX."
  (let (temp)
    (dolist (i (ebasic-parse-multistatement inlex))
      (push (ebasic-lex-to-sexp 
             (mapcar 'ebasic-eval-constants (ebasic-parse-group i)))
            temp))
    (reverse temp)))

(provide 'ebasic-parse)
