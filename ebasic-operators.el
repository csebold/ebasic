; ebasic-operators.el
; By Charles Sebold

(defun ebasic-operators/+ (arg1 arg2)
  "Add together ARG1 and ARG2 if numbers, or concatenate them if
strings."
  (if (listp arg1) (setq arg1 (ebasic-eval arg1)))
  (if (listp arg2) (setq arg2 (ebasic-eval arg2)))
  (cond
   ((and (numberp arg1)
         (numberp arg2))
    (+ arg1 arg2))
   ((and (stringp arg1)
         (stringp arg2))
    (ebasic-make-string (concat arg1 arg2)))
   ((or (listp arg1) (listp arg2))
    (list 'ebasic-mismatch/+ arg1 arg2))
   (t
    (ebasic-error "Type mismatch error."))))

(provide 'ebasic-operators)
