;(*add* 1 (*sub* 4 2))
;((lambda (a b) (+ a b)) 35 -10)
(let! fac
  (lambda (n)
    (if (< n 1)
        1
      (* n (fac (- n 1))))))
(fac 29)
fac
;(asd asd)

;(((lambda (x y z) (lambda () x)) 6 4 3))
;(+ (/ 6 4) 3)
