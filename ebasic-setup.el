; ebasic-setup.el
; By Charles Sebold

(defun ebasic-var-to-symbol (varname)
  "Convert ebasic variable name VARNAME to emacs symbol."
  (read (concat "ebasic-var/" varname)))

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
    (if (string-match "\\([[:alpha:]]+\\(\\$?\\)\\)(\\([[:digit:]]+\\)" i)
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
