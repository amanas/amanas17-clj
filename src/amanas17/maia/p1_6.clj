(ns amanas17.maia.p1-6
  (:require [amanas17.maia.p1-1 :refer :all]
            [amanas17.maia.p1-3 :refer :all]
            [amanas17.maia.p1-4 :refer :all]
            [amanas17.maia.p1-5 :refer :all]))


;; Ejercicio 1.22

(def all-ejemplos (mezclar ejemplos ejemplos2))

(defn resustitution
  "Calcula la precición del algoritmo de aprendizaje utilizando
   todos los casos para entrenar y evaluar"
  [entrenador interprete ejemplos]
  (let [concepto (entrenador ejemplos)
        ejemplos-sin-clase (map butlast ejemplos)
        extension (map (partial interprete concepto) ejemplos-sin-clase)]
    (calcula-precision ejemplos extension)))

(defn leave-one-out
  "Por cada ejemplo:
    - se toma el ejemplo como prueba
    - se toma el conjunto complementario de ejemplos como entrenamiento
    - se entrena el entrenador y se calcula su precisión en el ejemplo
   Todas estas pruebas se agregan para conseguir el resultado final"
  ([entrenador interprete [head & body :as ejemplos]]
   (let [precisiones (map (partial leave-one-out entrenador interprete ejemplos)
                          (range (count body)))]
     (/ (apply + precisiones) (count precisiones))))
  ([entrenador interprete [head & body :as ejemplos] index]
   (let [prueba [head (nth body index)]
         entrenamiento (concat [head] (take index body) (drop (inc index) body))
         concepto (entrenador entrenamiento)
         extension (map (partial interprete concepto) (map butlast prueba))]
     (calcula-precision prueba extension))))

(he-tardado 360 1.22)


;; Ejercicio 1.23
(defn holdout
  "Técnica de aprendizaje consistente en
    - entrenar con el conjunto entrenamiento
    - y evaluar con el conjunto evaluacion"
  [entrenador interprete entrenamiento evaluacion]
  (let [concepto (entrenador entrenamiento)
        extension (map (partial interprete concepto) (map butlast evaluacion))]
    (calcula-precision evaluacion extension)))

(he-tardado 90 1.23)


;; Ejercicio 1.24
(defn cross-validation
  "Técnica de apendizaje consistente en:
    - generar tantos folds en el conjunto entrenamiento como indica n-folds
    - después iterar del modo siguiente:
      - para cada fold entrenar en el conjunto de ejemplos que no están en el fold
      - para cada fold evaluar con el conjunto de ejemplos del fold
    y por último devolver la media de las precisiones"
  [entrenador interprete entrenamiento n-folds]
  (let [flds (folds entrenamiento n-folds)
        entre-eval-groups (for [n (range n-folds)]
                            (let [entrenamiento (->> (drop (inc n) flds)
                                                     (into (take n flds))
                                                     (reduce mezclar))
                                  evaluacion (nth flds n)]
                              [entrenamiento evaluacion]))
        precisiones (->> entre-eval-groups
                         (map (partial apply holdout entrenador interprete)))]
    (/ (apply + precisiones) (count precisiones))))

;; Se comprueba el test indicado en el material de la práctica, pero teniendo en
;; cuenta que tenemos 25 ejemplos en lugar de 3
(assert (= (cross-validation A0 A0i all-ejemplos 25)
           (leave-one-out A0 A0i all-ejemplos)))

(he-tardado 120 1.24)
