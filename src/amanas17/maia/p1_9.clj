(ns amanas17.maia.p1-9
  (:use        [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p1-4]
        [amanas17.maia.p1-5]))

;; Ejercicio 1.20
(def concepto-inicial '((+ 0) (- 0)))

(defn IIA1 [descripcion-atributos concepto ejemplo]
  (let [cm (apply hash-map (flatten concepto))]
    (seq (assoc cm (last ejemplo) (inc (get cm (last ejemplo)))))))

(he-tardado 30 1.20)

;; Ejercicio 21
(defn IA1
  "Implementación incremental del algoritmo A1"
  [ejemplos]
  (reduce (partial IIA1 (first ejemplos)) concepto-inicial (rest ejemplos)))

(he-tardado 15 1.21)

(he-tardado "una buena cantidad de" "autoencomendado de afinar todas las funciones correspondientes a la primera pr�ctica")
