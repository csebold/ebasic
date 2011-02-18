; ebasic-functions.el
; By Charles Sebold

(require 'ebasic-setup)

(defun ebasic/chr$ (int)
  "Convert INT to a single-char ASCII string."
  (ebasic-make-string (string int)))

(provide 'ebasic-functions)
