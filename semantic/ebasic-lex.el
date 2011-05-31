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
    (reverse temp)))

(provide 'ebasic-lex)
