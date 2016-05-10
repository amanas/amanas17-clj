(ns amanas17.maia.p2-9-2
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p1-6]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-4]
        [amanas17.maia.p2-6]
        [amanas17.maia.p2-7]))

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

(assert (= (nuevo-conceptoNB (first ejemplos))
           ['(+
              0
              ([soleado 0] [nublado 0] [lluvioso 0])
              [numerico 0 0]
              [numerico 0 0]
              ([no 0] [si 0])
              ([contento 0] [triste 0])
              ([relajado 0] [estresado 0])
              ([solvente 0] [insuficiente 0]))
            '(-
              0
              ([soleado 0] [nublado 0] [lluvioso 0])
              [numerico 0 0]
              [numerico 0 0]
              ([no 0] [si 0])
              ([contento 0] [triste 0])
              ([relajado 0] [estresado 0])
              ([solvente 0] [insuficiente 0]))]))

(defn INB
  "Actualiza de modo incremental el concepto Naive Bayes con los datos del ejemplo"
  [conceptoNB ejemplo]
  (let [clase (last ejemplo)
        remain-NB (remove (fn [[clazz & more]] (= clase clazz)) conceptoNB)
        update-NB (first (filter (fn [[clazz & more]] (= clase clazz)) conceptoNB))
        pairs (partition 2 (interleave (drop 2 update-NB) (butlast ejemplo)))
        updater (fn [[dist value]]
                  (prn dist)
                  (prn value)
                  ;; (if (= 'numerico (first dist))
                  ;;   [(nth dist 0) (+ (nth dist 1) value) (+ (nth dist 2) (Math/pow value 2))]
                  ;;   (map (fn [[nombre contador]]
                  ;;          [nombre (if (= nombre value) (inc contador) contador)])
                  ;       dist
                  )]
    (prn update-NB)
    (map updater pairs)))



(INB (nuevo-conceptoNB (first ejemplos)) (second ejemplos))
