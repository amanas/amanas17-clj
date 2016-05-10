(ns amanas17.maia.p2-9-1
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p1-6]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-4]
        [amanas17.maia.p2-6]
        [amanas17.maia.p2-7]))

(defn IB
  "Algoritmo de aprendizaje basado en instancias"
  [ejemplos]
  ejemplos)

(defn distancia
  "Calcula la distancia euclídea de un ejemplo a otro. Los ejemplos pueden tener
   o no clase"
  [ejemplo-x ejemplo-y]
  (let [pairs (->> ejemplo-x (interleave ejemplo-y) (partition 2))
        diff (fn [[x y]] (Math/pow (if (number? x) (- x y) (if (= x y) 0 1)) 2))]
    (->> pairs  (map diff) (reduce +) (Math/sqrt))))

(assert (= 5.196152422706632
           (distancia '(soleado 10 20 si)
                      '(soleado 15 21 no -))))

(he-tardado 30 2.34)

(defn IBi
  "Intérprete del algoritmo basado en instancias"
  [concepto-IB ejemplo-sin-clase]
  (concat ejemplo-sin-clase
          [(->> concepto-IB
                (sort-by (partial distancia ejemplo-sin-clase) <) first last)]))

(assert (= (IBi (rest ejemplos) (butlast (first (rest ejemplos))))
           '(nublado 15 90 si contento relajado insuficiente -) ))

(he-tardado 10 2.35)

(defn match-IB
  "Determina si la clase del ejemplo es la misma que la del vecino más cercano"
  [concepto-IB ejemplo]
  (= (IBi concepto-IB (butlast ejemplo)) ejemplo))

(assert (match-IB (rest ejemplos) (first (rest ejemplos))))

(he-tardado 30 2.36)
