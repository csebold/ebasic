; ebasic-setup.el
; By Charles Sebold

(require 'ebasic-parse)

(defvar ebasic-array-re
  "\\([[:alpha:]]+\\(\\$?\\)\\)(\\([[:digit:]]+\\)"
  ; FIXME: the array index will have to be replaced by a parser
  "Regexp to parse variable name, string flag, and index.")

(defvar ebasic-stack nil
  "Ebasic stack list for subroutines, for/next loops, etc.")

(defvar ebasic-vars nil
  "Ebasic index of variable names as strings.")

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

(defun ebasic-tokenize (add-to-hash hashtable)
  "Add ADD-TO-HASH to HASHTABLE and return a token."
  (let ((htc (hash-table-count hashtable)))
    (puthash htc add-to-hash hashtable)
    (concat "ebt" (string (+ 65 htc)) "ebt")))

(defun ebasic-detokenize (expression hashtable)
  "Replace tokens in EXPRESSION from HASHTABLE."
  (with-temp-buffer
    (insert expression)
    (goto-char (point-min))
    (while (re-search-forward "ebt\\([[:alpha:]]\\)ebt" nil t)
      (replace-match (gethash
                      (- (string-to-char (match-string 1)) 65)
                      hashtable) t t)
      (goto-char (point-min)))
    (buffer-string)))

(defun ebasic-string-escape (expression stack-hash)
  "Take all ebasic strings out of EXPRESSION and store them in
STACK-HASH, tokenizing the results."
  (save-match-data
    (cond
     ((string-match "\\(\".*?\"\\)" expression)
      (ebasic-string-escape
       (replace-match (ebasic-tokenize
                       (match-string 1 expression)
                       stack-hash) t t expression)
       stack-hash))
     ((string-match "\\([[:alpha:]]+\\$\\)" expression)
      (ebasic-string-escape
       (replace-match (ebasic-tokenize
                       (match-string 1 expression)
                       stack-hash) t t expression)
       stack-hash))
     (t
      expression))))

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

(defun ebasic-execute (instring)
  "Lex INSTRING and execute."
  (let ((ebasic-command-list (ebasic-parse-final (ebasic-lex instring))))
    (dotimes (ebasic-substatement-number (safe-length ebasic-command-list))
      (let ((i (nth ebasic-substatement-number ebasic-command-list)))
        (apply 'funcall (car i) (cdr i))))))

(provide 'ebasic-setup)
