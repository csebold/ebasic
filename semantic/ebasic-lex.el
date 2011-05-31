; ebasic-lex.el

(defvar ebasic-tokens
  '((" +"                                   . 'space)
    ("[[:alpha:]][[:alnum:]]*\\$?"          . 'identifier)
    ("[0-9]+\\|[0-9]+\\.[0-9]*\\|\\.[0-9]+" . 'number)
    ("\".*?\""                              . 'string)
    ("[+]"                                  . 'plus)
    ("[-]"                                  . 'minus)
    ("[*]"                                  . 'times)
    ("[/]"                                  . 'divided)
    ("\\^"                                  . 'exponent)
    ("="                                    . 'equals)
    ("<>"                                   . 'notequals)
    ("<"                                    . 'lt)
    ("<="                                   . 'le)
    ("=<"                                   . 'le)
    (">"                                    . 'gt)
    (">="                                   . 'ge)
    ("=>"                                   . 'ge)
    ("("                                    . 'lparen)
    (")"                                    . 'rparen)
    (","                                    . 'comma)
    (";"                                    . 'semicolon)
    (":"                                    . 'colon)
    ("'"                                    . 'apos))
  "Tokens for ebasic lexer routine.")

(defun ebasic-lex (instring)
  "Convert INSTRING into ebasic tokens."
  (if (string= instring "")
      nil
    (with-temp-buffer
      (insert instring)
      (goto-char (point-min))
      (let ((first-token
             (catch 'found
               (dolist (i ebasic-tokens)
                 (when (looking-at (car i))
                   (re-search-forward (car i) nil t)
                   (throw 'found (cons (cdr i) (match-string 0)))))
               'sn)))
        (if (eq first-token 'sn)
            'sn
          (cons first-token
                (ebasic-lex (buffer-substring (point)
                                              (point-max)))))))))

(provide 'ebasic-lex)
