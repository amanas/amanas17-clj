(ns amanas17.maia.p1-4-test
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-3]
        [amanas17.maia.p1-4]))


;; Ejercicio 1.12 - tests
(assert (= [21 81] (->> (separar 2/10 (into ['header] (range 100)))
                        (map count))))

;; ejercicio 1.13 - tests
(assert (let [fs (folds (into ['header] (range 100)) 10)]
          (and (= 10 (count fs))
               (= (repeat 10 11) (map count fs))
               (= (repeat 10  'header) (map first fs)))))

;; ejercicio 1.14 - tests
(assert (= 3 (count (stratify ejemplos 3))))
