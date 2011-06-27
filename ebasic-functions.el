; ebasic-functions.el
; By Charles Sebold

(require 'ebasic-setup)

(defun ebasic/add (arg1 arg2)
  "Add together ARG1 and ARG2 if numbers, or concatenate them if
strings."
  (cond
   ((and (numberp arg1)
         (numberp arg2))
    (+ arg1 arg2))
    ;; (if (or (floatp arg1) (floatp arg2))
    ;;     (+ 0.0 arg1 arg2)
    ;;   (+ arg1 arg2)))
   ((and (stringp arg1)
         (stringp arg2))
    (ebasic-make-string (concat arg1 arg2)))
   ((or (listp arg1) (listp arg2))
    (list 'ebasic-mismatch/+ arg1 arg2))
   (t
    (ebasic-error "Type mismatch error."))))

(defun ebasic/chr$ (int)
  "Convert INT to a single-char ASCII string."
  (ebasic-make-string (string int)))

(provide 'ebasic-functions)
