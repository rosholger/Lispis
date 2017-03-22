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
(let! a 'z)
(let! obj {(,a 1) (b 2) (c 3)})

(: obj a)

(+ (: obj z) (: obj b))