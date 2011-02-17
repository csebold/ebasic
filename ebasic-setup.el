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

(defun ebasic/clear (&optional stringspace)
  "Clear all variables.

STRINGSPACE is a dummy parameter for compatibility."
  (when (boundp 'ebasic-vars)
    (dolist (i ebasic-vars)
      (makunbound (ebasic-var-to-symbol i))))
  (setq ebasic-vars nil))

(defun ebasic/new ()
  "Clear program memory."
  (save-excursion
    (set-buffer (get-buffer-create "*ebasic program memory*"))
    (delete-region (point-min) (point-max))))

(defun ebasic/reset ()
  "Clear all variables and program memory."
  (ebasic/clear)
  (ebasic/new))

(defun ebasic/dim (vars)
  "Set aside array space for VARS."
  (dolist (i (split-string vars "[, ]" t))
    (if (string-match ebasic-array-re i)
        (let ((varname (match-string 1 i))
              (stringvarp (string= "$" (match-string 2 i)))
              (size (match-string 3 i)))
          (add-to-list 'ebasic-vars varname)
          (set (ebasic-var-to-symbol varname)
               (make-vector (string-to-number size)
                            (if stringvarp
                                ""
                              0)))))))

(provide 'ebasic-setup)
