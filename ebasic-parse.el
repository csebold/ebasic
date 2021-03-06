; ebasic-parse.el

; FIXME:

; The right way to parse this is not to assume a single line, but to
; assume that colons and newlines are basically the same thing, and
; start planning for more advanced block functionality now.  Ugh.

(require 'ebasic-setup)
(require 'ebasic-lex)

(defvar ebasic-operator-order
  '(exponent times divided plus minus
             equals ne lt le gt ge)
  "Defines operator order of execution in ebasic.")

; separate statements from expressions?  Everything else does.

(defvar ebasic-parse-expression-syntax
  ; operators
  '((ebasic/exp      (:expression exponent :expression)             (1 3))
    (ebasic/mul      (:expression times :expression)                (1 3))
    (ebasic/div      (:expression divided :expression)              (1 3))
    (ebasic/add      (:expression plus :expression)                 (1 3))
    (ebasic/sub      (:expression minus :expression)                (1 3))
    (ebasic/and      (:expression (identifier . "AND") :expression) (1 3))
    (ebasic/or       (:expression (identifier . "OR") :expression)  (1 3))
    (ebasic/eq       (:expression equals :expression)               (1 3))
    (ebasic/neq      (:expression ne :expression)                   (1 3))
    (ebasic/lt       (:expression lt :expression)                   (1 3))
    (ebasic/le       (:expression le :expression)                   (1 3))
    (ebasic/gt       (:expression gt :expression)                   (1 3))
    (ebasic/ge       (:expression ge :expression)                   (1 3))
    ; functions
    (ebasic/chr      ((identifier . "CHR$") group)       2)
    (ebasic/asc      ((identifier . "ASC") group)        2)
    )
  "Parse syntax for all possible expressions.

Syntax is:
\(ebasic/function (grammar) (arguments))

Example:
\(ebasic/add (:expression plus :expression) (1 3))

The above will find any case where two expressions are divided by
a lexed PLUS, and pass arguments 1 and 3 to the Emacs Lisp
function `ebasic/add' as its arguments.  Grammar elements that
must be in parentheses should be marked only as 'group' to make
sure it gets through `ebasic-parse'.  Grammar elements that can
themselves be BASIC expressions should be marked only by the
keyword :expression.  Arguments are members of the grammar list
starting with 1 (NOT zero!).  You can insert keywords into the
arguments and they will be passed verbatim to the ebasic
function.")

(defvar ebasic-parse-statement-syntax
  ; format: ebasic/FUNCTION syntax-sexp arg-test-function arg-conversion-function
  '(
    ;; (ebasic-parse-statements
    ;;                  (:statement colon :statement)       (1 3))
    (ebasic/let      (identifier equals :expression)     (1 3))
    (ebasic/new      ((identifier . "NEW"))              nil)
    (ebasic/clear    ((identifier . "CLEAR"))            nil)
    (ebasic/clear    ((identifier . "CLEAR") number)     2)
    (ebasic/goto     ((identifier . "GOTO") number)      2)
    (ebasic/gosub    ((identifier . "GOSUB") number)     2)
    (ebasic/stop     ((identifier . "STOP"))             nil)
    (ebasic/for      ((identifier . "FOR") identifier equals :expression (identifier . "TO") :expression
                      (identifier . "STEP") :expression) (2 4 6 8))
    (ebasic/for      ((identifier . "FOR") identifier equals :expression (identifier . "TO") :expression)
                     (2 4 6))
    (ebasic/next     ((identifier . "NEXT") identifier)  2)
    (ebasic/print    ((identifier . "PRINT") :rest)      2))
  "Parse syntax for all possible statements.")

(defun ebasic-literal (x)
  "Return literal value when possible."
  (cond
   ((and (listp x)
         (eq (car x) 'string)
         (stringp (cdr x)))
    (cdr x))
   ((and (listp x)
         (eq (car x) 'number)
         (and (stringp (cdr x))
              (string-match
               ebasic-number-literal-re
               (cdr x))))
    (string-to-number (cdr x)))
   (t
    x)))

(defun ebasic-eval-var (x)
  (if (and (listp x)
           (eq (car x) 'identifier))
      (ebasic-get-var (cdr x))
    x))

(defun ebasic-ungroup (x)
  "Remove 'group' identifiers from X, leaving lists intact."
  (if (and (listp x) x)
      (if (eq (car x) 'group)
          (ebasic-ungroup (cdr x))
        (if (and (= (safe-length x) 1) (listp (car x)))
            (list (ebasic-ungroup (car x)))
          (cons (car x) (ebasic-ungroup (cdr x)))))
    x))

(defun ebasic-parse-expression (x)
  "Parse expression X using `ebasic-parse-expression-syntax'."
  (ebasic-parse-expression-internal x))

(defun ebasic-parse-statement (x)
  "Parse expression X using `ebasic-parse-statement-syntax'."
  (let ((ebasic-parse-syntax ebasic-parse-statement-syntax))
    (ebasic-parse x)))

(defun ebasic-parse-statements (&rest args)
  "Parse each statement in ARGS."
  (mapcar 'ebasic-parse-statement args))

(defun ebasic-parse-literal-number (x)
  "Return literal number from lex X."
  (if (eq (car x) 'number)
      (string-to-number (cdr x))
    'tm))

(defun ebasic-eq-id (x y)
  "Determine if X and Y are equal identifiers."
  (if (and (listp x) (listp y))
      (and (eq (car x) (car y))
           (string= (cdr x) (cdr y)))
    (if (listp x)
        (and (eq (car x) y))
      (and (eq x (car y))))))

(defun ebasic-eq-types (x y)
  "Determine if X and Y are the same type."
  (or (eq x :expression)
      (eq y :expression)
      (eq (if (consp x) (car x) x) (if (consp y) (car y) y))))

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

(defun ebasic-first-instance (element list &optional testpred)
  "Return the index of the first time ELEMENT shows up in LIST.
If TESTPRED is not specified then it tests using `eq'."
  (unless testpred
    (setq testpred 'eq))
  (catch 'e-f-i
    (dotimes (i (safe-length list))
      (when (funcall testpred element (nth i list))
        (throw 'e-f-i i)))
    nil))

(defun ebasic-parse-equal (list1 list2)
  "Determine if LIST1 and LIST2 are equal in terms of parsing."
  (and (= (length list1) (length list2))
       (not
        (memq nil
              (let (temp)
                (dotimes (i (length list1))
                  (push (ebasic-eq-types (nth i list1)
                                         (nth i list2)) temp))
                temp)))))

(defun ebasic-match-parse (list matchlist)
  "If MATCHLIST is a sublist of LIST, then return a list of three
elements: the list up to MATCHLIST, the part that matched, and
the part after MATCHLIST; otherwise just return nil."
  (let (prematch
        (postmatch list))
    (catch 'e-m-p
      (while (>= (safe-length postmatch) (safe-length matchlist))
        (let ((test (butlast postmatch (- (safe-length postmatch)
                                          (safe-length matchlist)))))
          (if (ebasic-parse-equal matchlist test)
              (throw 'e-m-p
                     (list (reverse prematch)
                           test (last postmatch
                                      (- (safe-length postmatch)
                                         (safe-length matchlist)))))
            (push (pop postmatch) prematch))))
      (list list nil nil))))

(defun ebasic-assemble-args (func expr argsorder)
  "Assemble function FUNC with arguments from EXPR according to
ARGSORDER."
  (if argsorder
      (let (temp)
        (if (listp argsorder)
            (setq temp
                  (mapcar (lambda (y)
                            (if (keywordp y)
                                y
                              (let ((temp2 (nth (1- y) expr)))
                                (if (and (listp temp2)
                                         (eq (car temp2) 'group))
                                    (ebasic-parse-expression-internal temp2)
                                  (ebasic-literal temp2)))))
                          argsorder))
          (setq temp
                (if (and (listp temp)
                         (eq (car temp) 'group))
                    (ebasic-parse-expression-internal (nth (1- argsorder) expr))
                  (ebasic-literal (nth (1- argsorder) expr)))))
        (cons func (if (and (listp temp)
                            (eq (car temp) 'group))
                       (list (ebasic-parse-expression-internal temp))
                     temp)))
    (list func)))

(defun ebasic-parse-expression-internal (x)
  "Parse lex X using `ebasic-parse-expression-syntax'."
  (and x
       (catch 'e-p-e
         (dolist (i ebasic-parse-expression-syntax)
           (let* ((func (car i))
                  (parse (nth 1 i))
                  (argsorder (nth 2 i))
                  (matchp t)
                  (emp (ebasic-match-parse x parse))
                  expressionp groupp)
             (when (second emp)
               (throw 'e-p-e
                      (cond
                       ((and (not (car emp))
                             (not (third emp)))
                        (ebasic-assemble-args func (second emp) argsorder))
                       ((not (car emp))
                        (ebasic-parse-expression-internal
                         (append (list (ebasic-assemble-args func (second emp) argsorder))
                                 (third emp))))
                       ((not (third emp))
                        (ebasic-parse-expression-internal
                         (append (car emp)
                                 (list (ebasic-assemble-args func (second emp) argsorder)))))
                       (t
                        (ebasic-parse-expression-internal
                         (append (car emp)
                                 (list (ebasic-assemble-args func (second emp) argsorder))
                                 (third emp)))))))))
         x)))

(defun ebasic-parse (x)
  "Parse lex X using `ebasic-parse-syntax'."
  (ebasic-ungroup
   (if (eq (car x) 'group)
       (cons 'group (ebasic-parse (cdr x)))
     (if (and (listp x) (= 1 (safe-length x))
              (listp (cdr x)) (not (eq (cdr x) nil)))
         (ebasic-parse (car x))
       (catch 'found
         (dolist (i ebasic-parse-syntax)
;           (message "Testing grammar:%S against input:%S" i x)
           (let* ((func (car i))
                  (parse (nth 1 i))
                  (argsorder (nth 2 i))
                  (f-i-s (ebasic-first-instance (cadr parse)
                                                x 'ebasic-eq-id))
                  (matchp t)
                  expressionp groupp restp)
             ; test for multi-statements
             ;; (if (and (eq (car parse) :statement)
             ;;          f-i-s
             ;;          (>= (safe-length (cddr parse))
             ;;              (safe-length (nthcdr f-i-s x))))
                 
             ;; (when (memq :statement parse)
             ;;   (dotimes (j (length parse))
             ;;     (if 
             ;;   (throw '
             ; work through normal parsing
             (if (or (= (safe-length x) (length parse))
                     (and (or (memq :expression parse)
                              (memq :rest parse))
                          (>= (safe-length x) (length parse))))
                 (dotimes (j (length parse))
                   (when (and (symbolp (nth j parse))
                              (eq (nth j parse) 'group))
                     (setq groupp t))
;                   (message "Comparing grammar:%S with input:%S" (nth j parse) (nth j x))
                   (cond
                    ((and (keywordp (nth j parse))
                          (eq (nth j parse) :rest))
                     (setq restp t))
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
                                      (mapcar (lambda (y)
                                                (if (keywordp y)
                                                    y
                                                  (let ((temp2 (nth (1- y) x))
                                                        (parsed (nth (1- y) parse)))
                                                    (cond
                                                     ((and (listp temp)
                                                           (eq (car temp) 'group))
                                                      (ebasic-parse-expression temp2))
                                                     (restp
                                                      (mapcar 'ebasic-literal
                                                              (mapcar 'ebasic-parse-expression
                                                                      (nthcdr (1- argsorder) x))))
                                                     ((eq parsed :expression)
                                                      (ebasic-eval-var (ebasic-literal temp2)))
                                                     (t
                                                      temp2)))))
                                              argsorder))
                              (setq temp
                                    (cond
                                     ((and (listp temp)
                                           (eq (car temp) 'group))
                                      (ebasic-parse-expression (nth (1- argsorder) x)))
                                     (restp
                                      (mapcar 'ebasic-literal
                                              (mapcar 'ebasic-parse-expression (nthcdr (1- argsorder) x))))
                                     ((eq :expression (nth (1- argsorder) parse))
                                      (ebasic-eval-var (ebasic-literal (nth (1- argsorder) x))))
                                     (t
                                      (nth (1- argsorder) x)))))
                            (cons func (cond
                                        (restp temp)
                                        ((and expressionp
                                              (> 1 (safe-length temp)))
                                         (ebasic-parse-expression temp))
                                        ((and (listp temp)
                                              (eq (car temp) 'group))
                                         (list (ebasic-parse-expression temp)))
                                        (t temp))))
                        (list func))))))
         'sn)))))

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
      (push 
       (ebasic-parse-statement
        (ebasic-lex-to-sexp (ebasic-parse-group i)))
       temp))
    (reverse temp)))

(defun ebasic-execute (instring)
  "Lex INSTRING and execute."
  (let ((ebasic-command-list (ebasic-parse-final (ebasic-lex instring))))
    (dotimes (ebasic-substatement-number (safe-length ebasic-command-list))
      (let ((i (nth ebasic-substatement-number ebasic-command-list)))
        (apply 'funcall (car i)
               (if (and (atom (cddr i)) (cddr i))
                   (list (cdr i))
                 (cdr i)))))))

(provide 'ebasic-parse)
