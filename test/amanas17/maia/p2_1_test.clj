(ns amanas17.maia.p2-1-test
  (:require [clojure.test :refer :all]
            [amanas17.maia.p1-3 :refer :all]
            [amanas17.maia.p2-1 :refer :all]))

;; Ejercicio 2.1 - tests
(assert (every? test-ambivalente? [[] [*]]))

(assert (and (test-numerico? [])
             (test-numerico? [*])
             (test-numerico? [1])
             (test-numerico? [1 2])
             (test-numerico? [[1] 2])
             (test-numerico? [[1] [2]])
             (test-numerico? [1 [2]])))

(assert (and (test-nominal? [])
             (test-nominal? [*])
             (test-nominal? [:a])
             (test-nominal? [:a :b])))

(assert (and (match-nature? [] 1)
             (match-nature? [*] 1)
             (match-nature? [1] 1)
             (match-nature? [1 2] 1)
             (match-nature? [[1]] 1)
             (match-nature? [[1] 2] 1)
             (match-nature? [1 [2]] 1)
             (match-nature? [[1] [2]] 1)
             (match-nature? [:a] :b)
             (match-nature? [:a :b] :c)
             (match-nature? [] :a)
             (match-nature? [*] :a)))

(assert (and (not (match-ambivalente? [] :a))
             (match-ambivalente? [*] :a)))

(assert (and (match-nominal? [:a] :a)
             (match-nominal? [:a :b] :a)
             (not (match-nominal? [:a :b] :c))))

(assert (and (= [] (normalize-numerico []))
             (= [*] (normalize-numerico [*]))
             (= [:-inf 1] (normalize-numerico [:-inf 1]))
             (= [] (normalize-numerico [1 :-inf]))
             (= [1 :+inf] (normalize-numerico [1 :+inf]))
             (= [] (normalize-numerico [:+inf 1]))
             (= [:-inf 1] (normalize-numerico [[:-inf] 1]))
             (= [1 1] (normalize-numerico [1]))
             (= [1 1] (normalize-numerico [[1]]))
             (= [1 1] (normalize-numerico [1 1]))
             (= [] (normalize-numerico [1 [1]]))
             (= [] (normalize-numerico [[1] 1]))
             (= [1 2] (normalize-numerico [1 2]))
             (= [1 [2]] (normalize-numerico [1 [2]]))
             (= [[1] 2] (normalize-numerico [[1] 2]))
             (= [[1] [2]] (normalize-numerico [[1] [2]]))
             (= [] (normalize-numerico [2 1]))))

(assert (and (match-numerico? [1] 1)
             (match-numerico? [1 2] 1.5)
             (match-numerico? [[1] 2] 1)
             (match-numerico? [1 [2]] 2)
             (match-numerico? [[1] [2]] 1)
             (match-numerico? [1 1] 1)))

(assert (and (not (match-CL [[:soleado] [*] [10 40] [:si]]   [:soleado 30 40 :si]))
             (match-CL [[:soleado] [*] [10 [40]] [:si]] [:soleado 30 40 :si])
             (not (match-CL [[:soleado] [*] [10 40] [:si]]   [:soleado 30 25 :no]))
             (not (match-CL [[:soleado] [*] [10 40] [:si]]   [30 :soleado 25 :no]))))

;; Ejercicio 2.2 - tests
;; Nota:
;; hay un error tipográfico en este ejemplo en el material de estudio
(assert (= [:soleado 30 40 :si :+]
           (CLi [[:soleado] [*] [10 [40]] [:si]] [:soleado 30 40 :si])))

;; Ejercicio 2.4 - tests
(assert (= [[*] [*] [*] [*] [*] [*] [*]]
           (concepto-CL-mas-general (first ejemplos))))

;; Ejercicio 2.5 - tests
(assert (= [[] [] [] [] [] [] []]
           (concepto-CL-mas-especifico (first ejemplos))))
