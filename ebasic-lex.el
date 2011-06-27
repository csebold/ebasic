; ebasic-lex.el

(eval-and-compile
  (defvar ebasic-tokens
    '((" +"                                             . space)
      ("[[:alpha:]][[:alnum:]]*\\$?"                    . identifier)
      (ebasic-number-literal-re                         . number)
      ("\".*?\""                                        . string)
      ("[+]"                                            . plus)
      ("[-]"                                            . minus)
      ("[*]"                                            . times)
      ("[/]"                                            . divided)
      ("\\^"                                            . exponent)
      ("="                                              . equals)
      ("<>"                                             . ne)
      ("<"                                              . lt)
      ("<="                                             . le)
      ("=<"                                             . le)
      (">"                                              . gt)
      (">="                                             . ge)
      ("=>"                                             . ge)
      ("("                                              . lparen)
      (")"                                              . rparen)
      (","                                              . comma)
      (";"                                              . semicolon)
      (":"                                              . colon)
      ("'"                                              . apos))
    "Tokens for ebasic lexer routine."))

(defun ebasic-lex (instring)
  "Convert INSTRING into ebasic tokens."
  (let (temp)
    (with-temp-buffer
      (insert instring)
      (goto-char (point-min))
      (while (not (= (point) (point-max)))
        (let ((first-token
               (catch 'found
                 (dolist (i ebasic-tokens)
                   (when (looking-at (car i))
                     (re-search-forward (car i) nil t)
                     (throw 'found (cons (cdr i)
                                         (match-string 0)))))
                 'sn)))
          (if (eq first-token 'sn)
              (progn
                (setq temp (list 'sn))
                (goto-char (point-max)))
            (push first-token temp)))))
    (reverse (mapcar (lambda (x) (if (eq (car x) 'identifier)
                                     (cons 'identifier (upcase (cdr x)))
                                   x)) temp))))

(provide 'ebasic-lex)
