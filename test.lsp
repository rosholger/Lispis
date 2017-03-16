(defun! foldl (proc init lst)
   (if (null? lst)
     init
     (proc (car lst) (foldl proc init (cdr lst)))))
(defun! fac (n)
  (letfun! fac-inter (n)
    (if (< n 1)
      '(1)
      (cons n (fac-inter (- n 1)))))
  (foldl * 1 (fac-inter n)))
fac