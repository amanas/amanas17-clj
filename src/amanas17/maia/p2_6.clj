(ns amanas17.maia.p2-6
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-3]
        [amanas17.maia.p2-4]))

;; Ejercicio 2.20
(defn match-TC
  "Determina si una tabla de criterios es satisfecha para un ejemplo"
  [concepto-TC ejemplo-sin-clase]
  (and (= (count (rest concepto-TC)) (count ejemplo-sin-clase))
       (match-nature? [(rest concepto-TC) ejemplo-sin-clase])
       (->> ejemplo-sin-clase
            (interleave (rest concepto-TC))
            (partition 2)
            (filter (partial apply match?))
            count
            (<= (first concepto-TC)))))

(assert (and (not (match-TC [4 [soleado][**][10 40]   [si]] [soleado 30 40 si]))
             (match-TC [3 [soleado][**][10 40]   [si]] [soleado 30 40 si])
             (match-TC [2 [soleado][**][10 [40]] [si]] [soleado 30 40 si])
             (not (match-TC [4 [soleado][**][10 40]   [si]] [soleado 30 25 no]))
             (not (match-TC [3 [soleado][**][10 40]   [si]] [30 soleado 25 no]))))

;; Ejercicio 2.21
(defn TCi
  "Intérprete para tabla de criterios"
  [concepto-TC ejemplo-sin-clase]
  (conj ejemplo-sin-clase (if (match-TC concepto-TC ejemplo-sin-clase) '+ '-)))

(assert (= (TCi [4 [soleado] [**] [10 [40]] [si]] [soleado 30 40 si])
           [soleado 30 40 si '+]))
