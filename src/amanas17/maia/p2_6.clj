(ns amanas17.maia.p2-6
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-3]
        [amanas17.maia.p2-4]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Representación de conceptos TC (optativa) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(assert (and (not (match-TC '(4 (soleado) (*) (10 40) (si)) '(soleado 30 40 si)))
             (match-TC '(3 (soleado) (*) (10 40) (si)) '(soleado 30 40 si))
             (match-TC '(2 (soleado) (*) (10 (40)) (si)) '(soleado 30 40 si))
             (not (match-TC '(4 (soleado) (*) (10 40) (si)) '(soleado 30 25 no)))
             (not (match-TC '(3 (soleado) (*) (10 40) (si)) '(30 soleado 25 no)))))

(he-tardado 60 2.20)

;; Ejercicio 2.21
(defn TCi
  "Intérprete para tabla de criterios"
  [[umbral & concepto-CL :as concepto-TC] ejemplo-sin-clase]
  (concat ejemplo-sin-clase [(if (match-TC concepto-TC ejemplo-sin-clase) '+ '-)]))

(assert (= (TCi '(4 (soleado) (*) (10 (40)) (si)) '(soleado 30 40 si))
           '(soleado 30 40 si +)))

(he-tardado 20 2.21)

;; Ejercicio 2.22
(defn especializaciones-TC
  [[umbral & concepto-CL :as concepto-TC] metadatos ejemplo]
  (let [specs-CL (especializaciones-CL (vec concepto-CL) metadatos ejemplo)]
    (->> (range umbral (inc (count concepto-CL)))
         (map (fn [u] (map (partial cons u) specs-CL)))
         (apply concat)
         (cons (cons (inc umbral) concepto-CL))
         distinct)))
         
(he-tardado 60 2.22)
