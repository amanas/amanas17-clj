(ns amanas17.maia.p2-9-2
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p1-4]
        [amanas17.maia.p1-6]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-4]
        [amanas17.maia.p2-5]
        [amanas17.maia.p2-6]
        [amanas17.maia.p2-7]
        [amanas17.maia.p2-9-0]
        [amanas17.maia.p2-9-1]))


;;;;;;;;;;;;;;;;;;
;; Conceptos NB ;;
;;;;;;;;;;;;;;;;;;

(defn nuevo-conceptoNB
  "Crea un nuevo concepto Naive Bayes inicializado a 0 en todos sus valores"
  [metadatos]
  (loop [[[a-name a-desc :as atributo] & more] (butlast metadatos)
         template [0]]
    (if (nil? atributo)
      [(concat ['+] template) (concat ['-] template)]
      (if (= 'numerico a-desc)
        (recur more (concat template [['numerico 0 0]]))
        (recur more (concat template [(map (fn [v] [v 0]) a-desc)]))))))

(assert (= (nuevo-conceptoNB (first all-ejemplos))
           '((+
               0
               ([soleado 0] [nublado 0] [lluvioso 0])
               [numerico 0 0]
               [numerico 0 0]
               ([no 0] [si 0])
               ([contento 0] [triste 0])
               ([relajado 0] [estresado 0])
               ([solvente 0] [insuficiente 0]))
              (-
                0
                ([soleado 0] [nublado 0] [lluvioso 0])
                [numerico 0 0]
                [numerico 0 0]
                ([no 0] [si 0])
                ([contento 0] [triste 0])
                ([relajado 0] [estresado 0])
                ([solvente 0] [insuficiente 0])))))

;; Ejercicio 2.37
(defn INB
  "Actualiza de modo incremental el concepto Naive Bayes con los datos del ejemplo"
  [conceptoNB ejemplo]
  (let [clase (last ejemplo)
        remain-NB (remove (fn [[clazz & more]] (= clase clazz)) conceptoNB)
        update-NB (first (filter (fn [[clazz & more]] (= clase clazz)) conceptoNB))
        pairs (partition 2 (interleave (drop 2 update-NB) (butlast ejemplo)))
        updater (fn [[dist value]]
                  (if (= 'numerico (first dist))
                    [(nth dist 0) (+ (nth dist 1) value) (+ (nth dist 2) (Math/pow value 2))]
                    (map (fn [[nombre contador]]
                           [nombre (if (= nombre value) (inc contador) contador)])
                         dist)))]
    (concat remain-NB [(concat [(first update-NB) (inc (second update-NB))] (map updater pairs))])))

(he-tardado 60 2.37)


;; Ejercicio 2.38
(defn NB
  "Devuelve el concepto NB correspondiente a los ejemplos"
  [ejemplos]
  (loop [conceptoNB (nuevo-conceptoNB (first ejemplos))
         [ejemplo & more] (rest ejemplos)]
    (if (nil? ejemplo)
      conceptoNB
      (recur (INB conceptoNB ejemplo) more))))

(assert (= (NB all-ejemplos)
           '((+
               11
               ([soleado 4] [nublado 5] [lluvioso 2])
               [numerico 292 7988.0]
               [numerico 591 31859.0]
               ([no 9] [si 2])
               ([contento 10] [triste 1])
               ([relajado 8] [estresado 3])
               ([solvente 10] [insuficiente 1]))
              (-
                14
                ([soleado 1] [nublado 5] [lluvioso 8])
                [numerico 345 9749.0]
                [numerico 1115 89987.0]
                ([no 9] [si 5])
                ([contento 6] [triste 8])
                ([relajado 8] [estresado 6])
                ([solvente 5] [insuficiente 9])))))

(he-tardado 10 2.38)


;; Ejercicio 2.39

(defn media
  "Devuelve la media aritmética a partir de la suma de valores y el número de instancias"
  [x n] (/ x n))

(defn varianza
  "Devuelve la varianza a partir de la suma de los cuadrados de los valores, la media
   y el número de instancias"
  [x2 m n]
  (/ (+ x2 (* n (Math/pow m 2)) (* -2 m (* m n)))
     (dec n)))

(assert (= 2 (media 10 5)))
(assert (= 2.5 (varianza 30 2 5)))

(he-tardado 15 2.39)


;; Ejercicio 2.40

(defn norm-pdf
  "Devuelve la probabilidad de v para una normal de media m y varianza x2"
  [x m v]
  (* (/ 1 (Math/sqrt (* 2 Math/PI v)))
     (Math/exp (- (/ (Math/pow (- x m) 2)
                     (* 2 v))))))

(defn probabilidades
  "Devuelve una lista con las probabilidad de la clase por atributo.
   [P(c|vi)...]"
  [clase conceptoNB ejemplo-sin-clase]
  (let [[c+ n+ & more+] (first (filter (fn [[clazz & more]] (= clase clazz)) conceptoNB))
        [c- n- & more-] (first (remove (fn [[clazz & more]] (= clase clazz)) conceptoNB))]
    (loop [probs []
           [i & is] ejemplo-sin-clase
           [dist+ & dists+] more+
           [dist- & dists-] more-]
      (if (nil? i) probs
                   (if (number? i)
                     (let [[_ x+ x+2] dist+
                           [_ x- x-2] dist-
                           m+ (media x+ n+)
                           v+ (varianza x+2 m+ n+)
                           m- (media x- n-)
                           v- (varianza x-2 m- n-)
                           n (+ n+ n-)
                           x (+ x+ x-)
                           x2 (+ x+2 x-2)
                           m (media x n)
                           v (varianza x2 m n)
                           Pi|c (norm-pdf i m+ v+)
                           Pc (/ n+ n)
                           Pi (norm-pdf i m v)
                           Pc|i (/ (* Pi|c Pc) Pi)]
                       (recur (concat probs [Pc|i]) is dists+ dists-))
                     (let [s+ (second (first (filter (fn [[a b]] (= a i)) dist+)))
                           tot+ (reduce + (map second dist+))
                           s- (second (first (filter (fn [[a b]] (= a i)) dist-)))
                           tot- (reduce + (map second dist-))
                           Pi|c (/ s+ tot+)
                           Pc (/ n+ (+ n+ n-))
                           Pi (/ (+ s+ s-) (+ tot+ tot-))
                           Pc|i (/ (* Pi|c Pc) Pi)]
                       (recur (concat probs [Pc|i]) is dists+ dists-)))))))

(he-tardado 120 2.40)


;; Ejercicio 2.41

(defn NBi
  "Interprete de conceptos Naive Bayes"
  [conceptoNB ejemplo-sin-clase]
  (let [[c+ n+ & more+] (first (filter (fn [[clazz & more]] (= '+ clazz)) conceptoNB))
        [c- n- & more-] (first (remove (fn [[clazz & more]] (= '- clazz)) conceptoNB))
        P+ (/ n+ (+ n+ n-))
        P+|i (probabilidades '+ conceptoNB ejemplo-sin-clase)
        Pp+ (* P+ (reduce * P+|i))
        P- (- 1 P+)
        P-|i (probabilidades '- conceptoNB ejemplo-sin-clase)
        Pp- (* P- (reduce * P-|i))]
    (concat ejemplo-sin-clase [(if (>= Pp+ Pp-) '+ '-)])))

(defn match-NB
  "Devuelve true si la clase del ejemplo coincide con la deducida por el conceptoNB"
  [conceptoNB ejemplo]
  (= (last ejemplo) (last (NBi conceptoNB (butlast ejemplo)))))

(he-tardado 60 2.41)

;; Ejercicio 2.42

;; Tenemos que definir como hacer validación cruzada con stratificación
(def stratified-cross-validation (partial cross-validation stratify))


(comment (stratified-cross-validation HGS CLi all-ejemplos 10))
;; Precision: 106/120
(comment (stratified-cross-validation HTC CLi all-ejemplos 10))
;; Precision: 82/120
(comment (stratified-cross-validation PCP LUUi all-ejemplos 10))
;; Precision: 32/120
(comment (stratified-cross-validation LMS LUUi all-ejemplos 10))
;; Precision: 67/120
(comment (stratified-cross-validation IB IBi all-ejemplos 10))
;; Precision: 77/120
(comment (stratified-cross-validation NB NBi all-ejemplos 10))
;; Precision: 100/120

;; Curiosamente el algoritmo que más destaca para mis ejemplos es HGS.
;; Me resulta un poco sorprendente porque yo confiaba más en algoritmos
;; con una mayor componente matemática (PCP, LMS, NB) y no sólo basados en
;; comparaciones lógicas.
;; En cualquier caso, NB en la segunda posición también ofrece muy buenos
;; resultados.
;; El hecho que PCP ofrezca tan nefastos resultados me hace pensar que seguramente
;; tengo un bug en el código.
;; TODO: revisar PCP

(he-tardado 60 2.42)

;; Como de costrubre, no consigo que la computación acabe nunca para los ejemplos
;; ionosphere y agaricus-lepiota en la mayoría de los algoritmos

(comment (stratified-cross-validation HGS CLi ionosphere 10))
;; Precisión: La computación parece no acabar nunca
(comment (stratified-cross-validation HTC CLi ionosphere 10))
;; Precision: La computación parece no acabar nunca
(comment (stratified-cross-validation PCP LUUi ionosphere 10))
;; Precision: La computación parece no acabar nunca
(comment (stratified-cross-validation LMS LUUi ionosphere 10))
;; Precision: La computación parece no acabar nunca
(comment (stratified-cross-validation IB IBi ionosphere 10))
;; Precision: La computación parece no acabar nunca
(comment (stratified-cross-validation NB NBi ionosphere 10))
;; Precision: 4399/12600, bastante baja

(comment (stratified-cross-validation HGS CLi agaricus-lepiota 10))
;; Precisión: La computación parece no acabar nunca
(comment (stratified-cross-validation HTC CLi agaricus-lepiota 10))
;; Precision: La computación parece no acabar nunca
(comment (stratified-cross-validation PCP LUUi agaricus-lepiota 10))
;; Precision: La computación parece no acabar nunca
(comment (stratified-cross-validation LMS LUUi agaricus-lepiota 10))
;; Precision: La computación parece no acabar nunca
(comment (stratified-cross-validation IB IBi agaricus-lepiota 10))
;; Precision: La computación parece no acabar nunca
(comment (stratified-cross-validation NB NBi agaricus-lepiota 10))
;; Precision: La computación parece no acabar nunca
