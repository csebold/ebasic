; ebasic-parse.el

(defvar ebasic-operator-order
  '(exponent times divided plus minus
             equals ne lt le gt ge)
  "Defines operator order of execution in ebasic.")

; separate statements from expressions?  Everything else does.

(defvar ebasic-parse-expression-syntax
  ; operators
  '((ebasic/exp      (:expression exponent :expression)  (1 3))
    (ebasic/mul      (:expression times :expression)     (1 3))
    (ebasic/div      (:expression divided :expression)   (1 3))
    (ebasic/add      (:expression plus :expression)      (1 3))
    (ebasic/sub      (:expression minus :expression)     (1 3))
    (ebasic/eq       (:expression equals :expression)    (1 3))
    (ebasic/neq      (:expression ne :expression)        (1 3))
    (ebasic/lt       (:expression lt :expression)        (1 3))
    (ebasic/le       (:expression le :expression)        (1 3))
    (ebasic/gt       (:expression gt :expression)        (1 3))
    (ebasic/ge       (:expression ge :expression)        (1 3))
    ; functions
    (ebasic/chr      ((identifier . "CHR$") group)       2)
    (ebasic/asc      ((identifier . "ASC") group)        2)
    )
  "Parse syntax for all possible expressions.")

(defvar ebasic-parse-statement-syntax
  ; format: ebasic/FUNCTION syntax-sexp arg-test-function arg-conversion-function
  '((ebasic/let      (identifier equals :expression) (1 3))
    (ebasic/new      ((identifier . "NEW"))          nil)
    (ebasic/goto     ((identifier . "GOTO") number)  1))
  "Parse syntax for all possible statements.")

(defun ebasic-parse-expression (x)
  "Parse expression X using `ebasic-parse-expression-syntax'."
  (let ((ebasic-parse-syntax ebasic-parse-expression-syntax))
    (ebasic-parse x)))

(defun ebasic-parse-statement (x)
  "Parse expression X using `ebasic-parse-statement-syntax'."
  (let ((ebasic-parse-syntax ebasic-parse-statement-syntax))
    (ebasic-parse x)))

(defun ebasic-parse-literal-number (x)
  "Return literal number from lex X."
  (if (eq (car x) 'number)
      (string-to-number (cdr x))
    'tm))

(defun ebasic-eq-id (x y)
  "Determine if X and Y are equal identifiers."
  (and (eq (car x) (car y))
       (string= (cdr x) (cdr y))))

(defun ebasic-eq-types (x y)
  "Determine if X and Y are the same type."
  (eq (if (consp x) (car x) x) (if (consp y) (car y) y)))

(defun ebasic-separate (list elt)
  "Separate LIST by ELT, return the group before the first ELT
and the group starting with ELT."
  (let (before after found)
    (dolist (i list)
      (if found
          (push i after)
        (if (ebasic-eq-types i elt)
            (progn
              (push i after)
              (setq found t))
          (push i before))))
    (cons (reverse before) (list (reverse after)))))

(defun ebasic-parse-match (x grammar)
  "If X and GRAMMAR match, return X parsed in groups matching
GRAMMAR; otherwise return nil."
  (let (acc
        (temp x)
        (i 0))
    (while temp
      (if (>= i (length grammar))
          (progn
            (setq temp nil)
            (setq acc nil))
        (let ((current (pop temp))
              (current-g (nth i grammar))
              (current-g-cdr (nthcdr (1+ i) grammar)))
          (if (ebasic-eq-types current current-g)
              (push current acc)
            (if (and (eq current-g :expression)
                     (memq (caar current-g-cdr) temp))
                ; if we're on expression and there's an identifier afterwards
                (let ((temp2 (ebasic-separate temp (caar current-g-cdr))))
                  (if (or (eq nil (car temp2)) (eq nil (cdr temp2)))
                      (progn
                        (setq temp nil)
                        (setq acc nil))
                    (push (ebasic-parse-expression
                           (if (listp temp2) (car temp2) (list temp2)))
                          acc)))
              (if (and (eq current-g :expression)
                       (eq nil (caar current-g-cdr)))
                  ; expression runs to the end of the line
                  (progn
                    (push (ebasic-parse-expression (cons current temp)) acc)
                    (setq temp nil))
                (setq temp nil)
                (setq acc nil))))
          (setq i (1+ i)))))
    (reverse acc)))
            

; FIXME: need to be able to turn (identifier equals :expression
; some-other-lex) into a list of things that can be reached by number,
; so they can be passed to functions.  Then we can do something like:

; (ebasic/let (identifier equals :expression) nil (ebasic-parse-expression 3))

; and have it work as expected.

; could do it by unparsing and grouping - tell the system to keep adding
; to GROUP until it reaches NEXTIDENTIFIER or something like that.

(defun ebasic-parse (x)
  "Parse lex X using `ebasic-parse-syntax'."
  (when (eq (car x) 'group)
    (setq x (cdr x)))
  (catch 'found
    (dolist (i ebasic-parse-syntax)
      (let ((func (car i))
            (parse (nth 1 i))
            (argsorder (nth 2 i))
            (matchp t)
            expressionp)
        (if (or (= (length x) (length parse))
                (and (memq :expression parse)
                     (>= (length x) (length parse))))
            (dotimes (j (length parse))
              (cond
               ((and (keywordp (nth j parse))
                     (eq (nth j parse) :expression))
                (setq expressionp t))
               ((and (consp (nth j parse))
                     (not (ebasic-eq-id (nth j x) (nth j parse))))
                (setq matchp nil))
               ((and (atom (nth j parse))
                     (not (eq (car (nth j x)) (nth j parse))))
                (setq matchp nil))))
          (setq matchp nil))
        (when matchp
          (throw 'found
                 (if argsorder
                     (let (temp)
                       (if (listp argsorder)
                           (setq temp
                                 (mapcar (lambda (y) (nth (1- y) x))
                                         argsorder))
                         (setq temp (nth (1- argsorder) x)))
                       (cons func (if (or (and expressionp
                                               (> 1 (length temp)))
                                          (and (listp temp)
                                               (eq (car temp) 'group)))
                                      (ebasic-parse-expression temp)
                                    temp)))
                   (list func))))))
    'sn))

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
        (dolist (i inlex)
          (if (listp i)
              (unless (eq (car i) 'space)
                (push i temp))
            (push i temp)))
;        (push (read (concat "ebasic/" (cdar inlex))) output)
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

(defun ebasic-regroup-list (inlist start end first-atom)
  "Regroup INLIST to be INLIST up to START, then a list made up
of the item at FIRST-ATOM followed by the remaining items between
START and END, then a list of everything in INLIST after END."
  (let (temp group)
    (dotimes (i start)
      (push (nth i inlist) temp))
    (push (nth first-atom inlist) group)
    (dotimes (i (1+ (- end start)))
      (unless (= (+ i start) first-atom)
        (push (nth (+ start i) inlist) group)))
    (push (reverse group) temp)
    (dotimes (i (- (length inlist) end 1))
      (push (nth (+ i end 1) inlist) temp))
    (reverse temp)))

(defun ebasic-eval-expressions (inlex)
  "Convert TOKEN to expression, or return TOKEN."
  (let ((new-in
         (mapcar (lambda (x) (if (eq (car x) 'group)
                                 (ebasic-eval-expressions (cdr x))
                               x)) inlex)))
    (dolist (oper ebasic-operator-order)
      (while (memq oper (mapcar 'car new-in))
        ; FIXME: not elt, I need something to find the value
        (let ((this-oper (elt (mapcar 'car new-in) oper)))
          (setq new-in (ebasic-regroup-list new-in
                                            (- this-oper 1)
                                            (+ this-oper 1)
                                            this-oper)))))
    new-in))

(defun ebasic-parse-final (inlex)
  "Run all possible parsing on INLEX."
  (let (temp)
    (dolist (i (ebasic-parse-multistatement inlex))
      (push (ebasic-lex-to-sexp 
             (mapcar 'ebasic-eval-constants (ebasic-parse-group i)))
            temp))
    (reverse temp)))

(provide 'ebasic-parse)
