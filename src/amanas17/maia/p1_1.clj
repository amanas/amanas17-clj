(ns amanas17.maia.p1-1)

(defn he-tardado [m e]
  (prn (str "He tardado " m " minutos en el ejercicio " e)))

; ejercicio 2
(defn siguiente [l]
  (map inc l))

(assert (= (siguiente '(1 2)) '(2 3)))
(he-tardado 1 2)

; ejercicio 3
(defn sumas [l1 l2]
  (->> l2
       (interleave l1)
       (partition 2)
       (map (partial apply +))))

(assert (= (sumas '(1 2) '(3 4)) '(4 6)))
(he-tardado 10 3)

; ejercico 4
(defn factorial [n]
  (if (= 1 n)
    1
    (* n (factorial (dec n)))))

(assert (= (factorial 5) 120))
(he-tardado 20 4)

; ejercicio 5
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
      (->> cdf-1
           keys
           (filter (fn [[a b]] (<= a r b)))
           first
           (get cdf-1)))
    :default
    (first (obtener-al-azar (map (fn [v] (vector v 1)) elements)))))

(def example-1 [[:a 1] [:b 2] [:c 3]])
(assert (some (set example-1) [(obtener-al-azar example-1)]))
(assert (some #{1 2} [(obtener-al-azar [1 2])]))
(prn (frequencies (take 6000 (repeatedly #(obtener-al-azar example-1)))))
(he-tardado 120 5)

; ejercicio 6 - impementado en el 5
(def example-2 [:a :b :c])
(assert (obtener-al-azar example-2))
(prn (frequencies (take 6000 (repeatedly #(obtener-al-azar example-2)))))
(he-tardado 60 6)
