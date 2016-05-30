(ns amanas17.maia.p1-1)

(defn he-tardado [m e]
  (prn (str "He tardado " m " minutos en el ejercicio " e)))

;; Ejercicio 1.2
(defn siguiente [l]
  (map inc l))

(he-tardado 1 1.2)

;; Ejercicio 1.3
(defn sumas [l1 l2]
  (->> l2
       (interleave l1)
       (partition 2)
       (map (partial apply +))))

(he-tardado 10 1.3)

;; Ejercico 1.4
(defn factorial [n]
  (if (= 1 n) 1 (->> n dec factorial (* n))))

(he-tardado 20 1.4)

;; Ejercicio 1.5
(def cdf-1 (fn [pairs]
             (let [freqs (map second pairs)
                   total (apply + freqs)
                   inv-freqs (map #(/ % total) freqs)
                   cum-freqs (reductions + inv-freqs)
                   intervals (partition 2 1 (cons 0 cum-freqs))]
               (zipmap intervals pairs))))

(defn obtener-al-azar [[one & more :as elements]]
  (cond
    (empty? elements)
    nil
    (and (coll? one)
         (= 2 (count one))
         (number? (second one)))
    (let [cdf-1 (cdf-1 elements)
          r (rand)]
      (->> cdf-1 keys (filter (fn [[a b]] (<= a r b))) first (get cdf-1)))
    :default
    (first (obtener-al-azar (map (fn [v] (vector v 1)) elements)))))

(he-tardado 180 1.5)

;; Ejercicio 1.6 - impementado en el 5
(he-tardado 0 1.6)
