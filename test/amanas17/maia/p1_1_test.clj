(ns amanas17.maia.p1-1-test
  (:require [clojure.test :refer :all]
            [amanas17.maia.p1-1 :refer :all]))


;; Ejercicio 1.2 - tests
(assert (= (siguiente '(1 2)) '(2 3)))

;; Ejercicio 1.3 - tests
(assert (= (sumas '(1 2) '(3 4)) '(4 6)))

;; Ejercicio 1.4 - tests
(assert (= (factorial 5) 120))

;; Ejercicio 1.5 - tests
(def example-1 [[:a 1] [:b 2] [:c 3]])
(assert (some (set example-1) [(obtener-al-azar example-1)]))
(assert (some #{1 2} [(obtener-al-azar [1 2])]))
(prn (frequencies (take 6000 (repeatedly #(obtener-al-azar example-1)))))

;; Ejercicio 1.6 - test
(def example-2 [:a :b :c])
(assert (obtener-al-azar example-2))
(prn (frequencies (take 6000 (repeatedly #(obtener-al-azar example-2)))))
