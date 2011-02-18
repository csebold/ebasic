; ebasic-statements.el
; By Charles Sebold

(require 'ebasic-setup)

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

(defun ebasic/dim (args)
  "Set aside array space for variables in ARGS."
  (let ((my-args (ebasic-split-list args ",")))
    (dolist (i my-args)
      (let ((varname (car i))
            (size (car (cadr i))))
        (add-to-list 'ebasic-vars varname)
        (set (ebasic-var-to-symbol varname)
             (make-vector (1+ (string-to-number size))
                          (if (ebasic-stringvarp varname)
                              (ebasic-make-string "")
                            0)))))))

(provide 'ebasic-statements)
