; ebasic-parse.el

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
         (t
          (push i group))))
      (if (or group (not (= paren-level 0)))
          'unmatched-parentheses
        (reverse temp)))))

(provide 'ebasic-parse)
