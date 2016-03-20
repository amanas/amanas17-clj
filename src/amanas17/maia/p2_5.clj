(ns amanas17.maia.p2-5
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-3]
        [amanas17.maia.p2-4]))

;; Ejercicio 17
(defn score-CL
  [concepto-CL PSET NSET]
  (let [Pc (->> PSET (map (#(match-CL % concepto-CL))) (filter true?) count)
        Nc (->> NSET (map (#(match-CL % concepto-CL))) (filter false?) count)]
    (/ (+ Pc Nc) (+ (count PSET) (count NSET)))))
