(ns amanas17.maia.p1-4
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]))

(defn sort-randomly
  "Ordena aleatoriamente una lista"
  [xs]
  (loop [rxs []
         xs xs]
    (if (empty? xs) rxs
        (let [x (obtener-al-azar xs)
              xs (remove (partial = x) xs)]
          (recur (conj rxs x) xs)))))

;; ejercicio 12
(defn separar
  "Separa la lista de entrada en dos manteniendo la proporción
   de cardinalidad indicada por p
   Añade aleatoriedad a la generación de las listas
   Asume que la lista de entrada tiene cabecera de atributos
   Las listas que devuelve también tienen cabecera de atributos"
  [p [head & body :as list]]
  (let [m (* p (count body))
        rxs (sort-randomly body)]
    [(into [head] (take m rxs)) (into [head] (drop m rxs))]))

(assert (= [21 81] (->> (separar 2/10 (into [:header] (range 100)))
                        (map count))))
(he-tardado 240 12)

;; ejercicio 13
(defn folds
  "Separa en n folds una lista.
   Añade aleatoriedad
   Asume que la lista tiene cabecera con descripción de atributos
   Los folds que devuelve también tienen cabecera de atributos"
  [xs n]
  (if (= 1 n) [xs]
      (let [[l r] (separar (/ 1 n) xs)]
        (concat [l] (folds r (dec n))))))

(assert (let [fs (folds (into [:header] (range 100)) 10)]
          (and (= 10 (count fs))
               (= (repeat 10 11) (map count fs))
               (= (repeat 10  :header) (map first fs)))))
(he-tardado 60 13)

;; ejercicio 14
(defn stratify
  "Separa en n folds una lista de modo que la proporción de ejemplos
   de cada clase se mantiene similar en cada fold
   Asume que la lista de entrada tiene cabecera con descripción de atributos
   Los folds que devuelve también tienen cabecera con descripción de atributos"
  [n [head & body :as list]]
  (let [groups (group-by identity (atributo :clase list))
        weights (into {} (for [[k v] groups] [k (count v)]))
        weighted-body (->> body (map (fn [x] [x (get weights (last x))])))
        weighted-folds (folds weighted-body n)]
    (map (fn[wf] (map first wf)) weighted-folds)))

(assert (= 3 (count (stratify 3 ejemplos))))
(he-tardado 240 14)
