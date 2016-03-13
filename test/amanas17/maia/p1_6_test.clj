(ns amanas17.maia.p1-6-test
  (:require [clojure.test :refer :all]
            [amanas17.maia.p1-4 :refer :all]
            [amanas17.maia.p1-5 :refer :all]
            [amanas17.maia.p1-6 :refer :all]))


;; Ejercicio 1.22 - tests
(prn "Precisión con resustitución  A0 A0i" (resustitution  A0 A0i all-ejemplos))
(prn "Precisión con resustitución  A1 A1i" (resustitution  A1 A1i all-ejemplos))
(prn "Precisión con leave-one-out  A0 A0i" (leave-one-out  A0 A0i all-ejemplos))
(prn "Precisión con leave-one-out  A1 A1i" (leave-one-out  A1 A1i all-ejemplos))


;; Ejercicio 1.23 - tests
(let [[entrenamiento evaluacion] (separar 2/3 all-ejemplos)]
  (prn "Precisión con holdout  A0 A0i" (holdout  A0 A0i entrenamiento evaluacion))
  (prn "Precisión con holdout  A1 A1i" (holdout  A1 A1i entrenamiento evaluacion)))
