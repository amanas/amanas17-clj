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
  [[umbral & concepto-CL :as concepto-TC] ejemplo-sin-clase]
  (and (= (count concepto-CL) (count ejemplo-sin-clase))
       (match-nature? [concepto-CL ejemplo-sin-clase])
       (->> ejemplo-sin-clase
            (interleave concepto-CL)
            (partition 2)
            (filter (partial apply match?))
            count
            (<= umbral))))

(assert (and (not (match-TC [4 [soleado][**][10 40] [si]] [soleado 30 40 si]))
             (match-TC [3 [soleado][**][10 40]      [si]] [soleado 30 40 si])
             (match-TC [2 [soleado][**][10 [40]]    [si]] [soleado 30 40 si])
             (not (match-TC [4 [soleado][**][10 40] [si]] [soleado 30 25 no]))
             (not (match-TC [3 [soleado][**][10 40] [si]] [30 soleado 25 no]))))

;; Ejercicio 2.21
(defn TCi
  "Intérprete para tabla de criterios"
  [[umbral & concepto-CL :as concepto-TC] ejemplo-sin-clase]
  (conj ejemplo-sin-clase (if (match-TC concepto-TC ejemplo-sin-clase) '+ '-)))

(assert (= (TCi [4 [soleado] [**] [10 [40]] [si]] [soleado 30 40 si])
           [soleado 30 40 si '+]))

;; Ejercicio 2.22
;; TODO: contemplar las especializaciones en varios atributos
(defn especializaciones-TC
  [[umbral & concepto-CL :as concepto-TC] metadatos ejemplo]
  (let [specs-CL (especializaciones-CL (vec concepto-CL) metadatos ejemplo)
        a (->> (range umbral (inc (count concepto-CL)))
               (map (fn [u] (map (partial cons u) specs-CL)))
               (apply concat)
               distinct)
        b (concat (map (fn [s] (cons umbral s)) specs-CL)
                  (map (fn [s] (cons (inc umbral) s)) specs-CL))
        c (map (fn [s]  (cons (inc umbral) s)) specs-CL)]
    a))

;; TODO: test me
;; (def meta (first ejemplos))
;; (def ej+  (->> ejemplos rest (filter (comp (partial = '+) last))))
;; (def ej- (->> ejemplos rest (filter (comp (partial = '-) last))))
;; (take 5 (drop 25 (especializaciones-TC [0 [**] [**] [**] [**] [**] [**] [**]] meta (first ej-))))
