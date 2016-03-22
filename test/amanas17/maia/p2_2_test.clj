(ns amanas17.maia.p2-2-test
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]))

;; Ejercicio 2.6 - tests
(assert (and (test-ambivalente>= [**] [**])
             (test-ambivalente>= [**] [])
             (test-ambivalente>= [] [])
             (not (test-ambivalente>= [] [**]))))

(assert (and (test-numerico>= [1 2] [3 4])
             (test-numerico>= [1 4] [2 3])
             (test-numerico>= [1 3] [2 4])
             (not (test-numerico>= [2 3] [1 4]))
             (test-numerico>= [1 2] [1 2])
             (test-numerico>= [1] [1])
             (test-numerico>= [1] [1 1])
             (test-numerico>= [1 1] [1])
             (test-numerico>= [1 1] [1 1])
             (not (test-numerico>= [[1] 1] [1 1]))
             (not (test-numerico>= [1 [1]] [1 1]))
             (test-numerico>= [1 1] [[1] 1])
             (test-numerico>= [1 1] [1 [1]])
             (not (test-numerico>= [1 2] [[1] 2]))
             (test-numerico>= [1 2] [1 [2]])
             (test-numerico>= [-inf 1] [1 1])
             (not (test-numerico>= [+inf 1] [1 1]))
             (test-numerico>= [1 +inf] [1 1])
             (not (test-numerico>= [1 -inf] [1 1]))))

(assert (and (test-nominal>= ['a 'b] ['a])
             (test-nominal>= [**] ['a])
             (test-nominal>= ['a] [])
             (test-nominal>= ['a 'b] ['a 'c])
             (not (test-nominal>= ['a] ['a 'b]))
             (not (test-nominal>= [] ['a]))
             (test-nominal>= ['a] [])))

(assert (and (test-CL>= [] [])
             (not (test-CL>= [] [**]))
             (test-CL>= [**] [])
             (test-CL>= [**] [**])
             (test-CL>= [lluvioso] [soleado])
             (test-CL>= [lluvioso] [])
             (not (test-CL>=  []   [lluvioso]))
             (test-CL>= [25 30] [26])
             (test-CL>=  [25 30] [21 25])
             (not (test-CL>= [26] [25 30]))))

;; Ejercicio 2.7 - tests
(assert (and (concepto-CL>= [[lluvioso] [2 10]] [[soleado] [10 20]])
             (concepto-CL>= [[lluvioso] [2 35]] [[soleado] [23]])
             (not (concepto-CL>= [[soleado][23]] [[lluvioso][2 35]]))))

;; Ejercicio 2.8 - tests
(assert (and (=  1 (cmp-concepto-CL [[lluvioso] [2 35]] [[soleado]  [23]]))
             (=  0 (cmp-concepto-CL [[lluvioso] [2 10]] [[soleado]  [10 20]]))
             (= -1 (cmp-concepto-CL [[soleado]  [23]]   [[lluvioso] [2 35]]))))
