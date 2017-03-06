(defmacro! defun! (name params . body)
  (quasiquote (define! (unquote name)
                (lambda (unquote params)
                  (unquote-splice body)))))

(defun! fac (n)
  (if (< n 1)
    1
    (* n (fac (- n 1)))))