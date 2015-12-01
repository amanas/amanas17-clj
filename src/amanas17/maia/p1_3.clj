(ns amanas17.maia.p1-3
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-2]))

;; ejercicio 12
(defn separar [p xs]
  (->> (repeatedly (fn [] (obtener-al-azar [[:r1 p] [:r2 (- 1 p)]])))
       (zipmap xs)
       (group-by val)
       ((juxt :r1 :r2))
       (map (partial map first))))
;; para listas pequeñas, separar no ofrece aleatoriedad suficiente como para
;; obtener los resultados que necesitamos. Por eso la sobreescribo, atendiendo
;; a un implementación algo más determinista
(defn separar [p xs]
  (let [m (* p (count xs))]
    (->> xs shuffle ((juxt (partial take m) (partial drop m))))))
;; finalmente, lo dejo utilizando la función obtener al hazer,
;; pero el rendimiento es bajo si la lista de ejemplos es grande
(defn separar [p xs]
  (loop [xs1 []
         xs2 []]
    (if (= (+ (count xs1) (count xs2)) (count xs))
      [xs1 xs2]
      (let [x (first (drop-while (fn [x] (or (some #{x} xs1) (some #{x} xs2)))
                                 (repeatedly #(obtener-al-azar xs))))]
        (if (< (rand) p)
          (recur (conj xs1 x) xs2)
          (recur xs1 (conj xs2 x)))))))

(assert (< (->> (separar 2/10 (range 100))
                (map count)
                (apply /)
                (- 0.2)
                Math/abs)
           0.2))
(he-tardado 240 12)

;; ejercicio 13
;; llamo a la función separar, así que implicitamente estoy utilizando la función
;; obtener al hazar, salvo por el hecho de que no la utilizo de acuerdo con el
;; comentario del ejercicio 12
(defn folds [xs n]
  (if (zero? n) xs
      (let [[l r] (separar (/ 1 n) xs)]
        (concat [l] (folds r (dec n))))))

(assert (let [fs (folds (range 100) 10)]
          (and (= 10 (count fs))
               (= (repeat 10 10) (map count fs)))))
(he-tardado 60 13)

;; 14
(defn stratify [n xs])
(let [clases (distinct (atributo :clase ejemplos))
      groups (->> ejemplos rest (group-by last) vals)
      group-folds (map #(folds % 4) groups)]
  (interleave group-folds))

(->> groups
     (map #(folds % 2))
     ffirst
     (map interleave))
(interleave [1 2] [3 4] [ 5 ])


(separar 0.89 (range 100))
