;(*add* 1 (*sub* 4 2))
;((lambda (a b) (+ a b)) 35 -10)
((lambda ()
   (let! fib (lambda (n)
               (if (< n 1)
                   1
                 (* n (fib (- n 1))))))
   (fib 6)))

;(((lambda (x y z) (lambda () x)) 6 4 3))
;(+ (/ 6 4) 3)
