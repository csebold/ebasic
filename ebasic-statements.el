; ebasic-statements.el
; By Charles Sebold

(require 'ebasic-setup)

(defun ebasic/clear (&optional stringspace)
  "Clear all variables.

STRINGSPACE is a dummy parameter for compatibility."
  (when (boundp 'ebasic-vars)
    (dolist (i ebasic-vars)
      (makunbound (ebasic-var-to-symbol i))))
  (setq ebasic-stack nil)
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

(defun ebasic/print (&rest args)
  "Print ARGS."
  ; FIXME
  (with-current-buffer (get-buffer-create "*ebasic output*")
    (goto-char (point-max))
    (let ((i 0) temp)
      (while (< i (safe-length args))
        (cond
         ((and (consp (nth i args))
               (ebasic-eq-id 'comma (nth i args)))
          (push "\011" temp))
         ((and (consp (nth i args))
               (ebasic-eq-id 'semicolon (nth i args)))
          (push 'semicolon temp))
         (t
          (push (ebasic-eval-var (ebasic-literal (nth i args))) temp)))
        (setq i (1+ i)))
      (insert
       (apply 'concat
              (mapcar (lambda (x) (if (numberp x) (number-to-string x) x))
                      (reverse
                       (remq 'semicolon
                             (if (eq (car temp) 'semicolon)
                                 (cdr temp)
                               (append '("\n") temp))))))))))

(defun ebasic/for (var start end &optional step)
  "Ebasic for loop."
  (unless step (setq step 1))
  (when (or (and (> 0 step) (>= start (+ step end)))
            (and (< 0 step) (<= start (+ step end))))
    (progn
      (push (list 'for var end step ebasic-substatement-number) ebasic-stack)
      (ebasic-set-var (cdr var) start))))

(defun ebasic/next (var)
  "Ebasic NEXT statement."
  (let ((not-found t))
    (while not-found
      (let* ((stack-search (pop ebasic-stack))
             (varname (cdr (nth 1 stack-search)))
             (end (nth 2 stack-search))
             (step (nth 3 stack-search))
             (esn (nth 4 stack-search)))
        (if (and (eq 'for (car stack-search))
                 (ebasic-eq-id (cadr stack-search) var))
            (progn
              (setq not-found nil)
              (ebasic-set-var varname
                              (+ (ebasic-get-var varname)
                                 step))
              (when (<= (ebasic-get-var varname) end)
                (push stack-search ebasic-stack)
                (setq ebasic-substatement-number esn)))
          (unless stack-search
            ; next without for error
            (setq not-found nil)))))))

(provide 'ebasic-statements)
