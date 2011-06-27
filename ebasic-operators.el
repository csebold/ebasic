; ebasic-operators.el
; By Charles Sebold

(require 'calc)

(defun ebasic-operators/- (arg1 arg2)
  "Subtract ARG2 from ARG1."
  (if (listp arg1) (setq arg1 (ebasic-eval arg1)))
  (if (listp arg2) (setq arg2 (ebasic-eval arg2)))
  (if (and (numberp arg1)
           (numberp arg2))
      (- arg1 arg2)
    (ebasic-error "Type mismatch error.")))

(provide 'ebasic-operators)
