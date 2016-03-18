(ns amanas17.maia.p2-4
  (:use [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-3]))

(defn without
  "Removes an element from a collection"
  [x coll]
  (remove (partial = x) coll))

(defn count-matchings
  "Cuenta los ejemplos que incorpora el concepto"
  [concepto ejemplos]
  (->> ejemplos (map butlast) (map (partial match-CL concepto)) (remove false?) count ))

(defn specializations-matching-less-than
  "Devuelve las especializaciones inmediatas del concepto h que satisfacen menos
   ejemplos que el propio h"
  [h ejemplos meta]
  (let [h-matching (count-matchings h ejemplos)
        specs (map (partial especializaciones-CL h meta) ejemplos)
        s-matchings (map (fn [s] (count-matchings s ejemplos)) specs)]
    (filter (fn [s] (< (count-matchings s ejemplos) h-matching)) specs)))

(defn none-more-general?
  "Devuelve true ningún concepto de conceptos es más general que el concepto"
  [conceptos concepto]
  (->> conceptos
       (map (fn [c] (cmp-concepto-CL c concepto)))
       (filter pos?)
       empty?))

(defn EGS0
  "Implementación del algoritmo EGS de inducción exaustiva
   de conjunciones lógicas.
   Asume que el primer elemento de PSET y NSET son los metadatos"
  [PSET NSET CSET HSET]
  (let [meta (first PSET)
        pSET (rest PSET)
        nSET (rest NSET)
        [HSET CSET] (loop [[h & more] HSET
                           hSET HSET
                           cSET CSET]
                      (cond (nil? h) [hSET cSET]
                            (every? (partial match-CL h) (map butlast pSET)) (recur more hSET cSET)
                            (some (partial match-CL h) (map butlast pSET)) (recur more (without h hSET) cSET)
                            :else (recur more (without h hSET) (cons h cSET))))]
    (if (empty? HSET) CSET
        (let [NEWSET
              (loop [[h & more] HSET
                     NEWSET []]
                (if (nil? h) NEWSET
                    (recur more
                           (loop [[s & more] (specializations-matching-less-than h nSET meta)
                                  NEWSET NEWSET]
                             (cond (nil? s) NEWSET
                                   (none-more-general? CSET s) (recur more (cons s NEWSET))
                                   :else (recur more NEWSET))))))]
          (EGS0 PSET NSET CSET NEWSET)))))

(defn EGS
  "Devuelve un concepto al azar con EGS0 de aplicado a los ejemplos
   tomando el concepto más específico como CSET y el más general como HSET"
  [ejemplos]
  (let [meta (first ejemplos)
        ejemplos+ (remove (partial = :-) (rest ejemplos))
        ejemplos- (remove (partial = :-) (rest ejemplos))
        [one & more :as egs0] (EGS0 (cons meta  ejemplos+)
                                    (cons meta ejemplos-)
                                    []
                                    [(concepto-CL-mas-general meta)])]
    (rand-nth egs0)))


;(clojure.pprint/pprint)
;(take 5)
(EGS ejemplos)
