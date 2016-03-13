(ns amanas17.maia.p1-9-test
  (:require [clojure.test :refer :all]
            [amanas17.maia.p1-3 :refer :all]
            [amanas17.maia.p1-9 :refer :all]))


;; Ejercicio 1.20 - tests
(assert (= [[:- 0] [:+ 1]]
           (IIA1 (first ejemplos) concepto-inicial [:nublado 25 80 :no :+])))


;; Ejercicio 1.21 - tests
(assert (= [[:- 13] [:+ 7]]
           (IA1 ejemplos)))
