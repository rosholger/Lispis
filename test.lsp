;(defun! fac (n)
  ;(if (< n 1)
      ;1
    ;(* n (fac (- n 1)))))
;fac
;
;(let! res 0)
;(for (0 (< it 10) (set! it (+ it 1)))
     ;(for (0 (< it 10) (set! it (+ it 1)))
          ;(set! res (+ res 1))))
;res
;(let! v ['a 2 3 4])
;(let! r 0);
;(:! v ,r 10)
;(: v ,r)

;(let! coordinate {(+ (lambda (this other)
                       ;{(x (+ (: this x) (: other x)))
                        ;(y (+ (: this y) (: other y)))}))
                  ;(type 'coordinate)})
;
;(let! v1 {(*proto* coordinate) (x 10) (y 20)})
;(let! v2 {(*proto* coordinate) (x 5) (y 15)})
;
;(+ v1 v2)

(let! obj {(*proto* {(a 100)}) (b 10)})
(:! obj a 10)
(+ (: obj a) (: obj b))