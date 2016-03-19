(ns amanas17.maia.p1-9-test
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-3]
        [amanas17.maia.p1-9]))


;; Ejercicio 1.20 - tests
(assert (= [[-- 0] [++ 1]]
           (IIA1 (first ejemplos) concepto-inicial [nublado 25 80 no ++])))


;; Ejercicio 1.21 - tests
(assert (= [[-- 11] [++ 9]]
           (IA1 ejemplos)))
