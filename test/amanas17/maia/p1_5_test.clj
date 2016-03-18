(ns amanas17.maia.p1-5-test
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-3]
        [amanas17.maia.p1-5]))


;; Ejercicio 1.16 - tests
(assert (= (calcula-precision ejemplos extension) 13/20))
(assert (= (calcula-error ejemplos extension) 7/20))

;; Ejercicio 1.17 - tests
(assert (= precision2-A0-A0i 3/5))
(assert (= error2-A0-A0i 2/5))

;; Ejercicio 1.18 - tests
(assert  (= (A1 ejemplos) [[++ 7] [-- 13]]))
(prn "One classification" (clasifica-con-A1-A1i ejemplos2))
