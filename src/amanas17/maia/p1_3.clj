(ns amanas17.maia.p1-3
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-2]))

(defn sort-randomly [xs]
  (loop [rxs []
         xs xs]
    (if (empty? xs) rxs
        (let [x (obtener-al-azar xs)
              xs (remove (partial = x) xs)]
          (recur (conj rxs x) xs)))))

;; ejercicio 12
(defn separar [p xs]
  (let [m (* p (count xs))
        rxs (sort-randomly xs)]
    [(take m rxs) (drop m rxs)]))

(assert (= [20 80] (->> (separar 2/10 (range 100))
                        (map count))))
(he-tardado 240 12)

;; ejercicio 13
(defn folds [xs n]
  (if (zero? n) xs
      (let [[l r] (separar (/ 1 n) xs)]
        (concat [l] (folds r (dec n))))))

(assert (let [fs (folds (range 100) 10)]
          (and (= 10 (count fs))
               (= (repeat 10 10) (map count fs)))))
(he-tardado 60 13)

;; ejercicio 14
(defn stratify
  "Asume que la lista tiene cabecer con descripción de atributos"
  [n xs]
  (let [g-xs (group-by identity (atributo :clase xs))
        o-xs (into {} (for [[k v] g-xs] [k (count v)]))
        instances (rest xs)
        w-xs (->> instances (map (fn [x] [x (get o-xs (last x))])))
        f-xs (folds w-xs n)]
    (map #(map first %) f-xs)))

(assert (= 3 (count (stratify 3 ejemplos))))
(he-tardado 240 14)
