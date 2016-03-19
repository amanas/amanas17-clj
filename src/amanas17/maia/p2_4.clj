(ns amanas17.maia.p2-4
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-3]))

(defn with
  "Adds an element to a collection"
  [x coll]
  (into coll x))

(defn without
  "Removes an element from a collection"
  [x coll]
  (remove (partial = x) coll))

(defn count-matchings
  "Cuenta los ejemplos que incorpora el concepto"
  [concepto ejemplos]
  (->> ejemplos (map butlast) (map (partial match-CL concepto)) (remove false?) count ))

(defn specs-matching-less-than
  "Devuelve las especializaciones inmediatas del concepto h que satisfacen menos
   ejemplos que el propio h"
  [h ejemplos meta]
  (let [h-matching (count-matchings h ejemplos)
        specs (->> ejemplos
                   (map (partial especializaciones-CL h meta)))]
    (filter (fn [s] (<= (count-matchings s ejemplos) h-matching)) specs)))

(defn none-more-general?
  "Devuelve true ningún concepto de conceptos es más general que el concepto"
  [conceptos concepto]
;  (prn "conceptos")
;  (clojure.pprint/pprint conceptos)
;  (prn "concepto" concepto)

  (->> conceptos
       (map (fn [c] (cmp-concepto-CL c concepto)))
       (filter pos?)
       empty?))

(defn EGS0
  "Implementación del algoritmo EGS de inducción exaustiva
   de conjunciones lógicas.
   Asume que el primer elemento de PSET y NSET son los metadatos"
  [PSET NSET CSET HSET]
 ;; (prn "HSET11111111111111111" CSET)
  (let [meta (first PSET)
        pSET (rest PSET)
        pSEText (map butlast pSET)
        nSET (rest NSET)
        nSEText (map butlast nSET)
        [CSET HSET] (loop [[h & more] HSET
                           cSET CSET
                           hSET HSET]
                      (if (nil? h) [cSET hSET]
                          (let [addToC? (not-any? (partial match-CL h) nSEText)
                                delFrH? (or addToC?
                                            (not-every? (partial match-CL h) pSEText))
                                newCSET (if addToC? (with h cSET) cSET)
                                newHSET (if delFrH? (without h hSET) hSET)]
                            (recur more newCSET newHSET))))]
;;        (prn "HSET22222222222222222" CSET)
    (if (empty? HSET) CSET
        (let [NEWSET
              (loop [[h & more] HSET
                     newSET []]
                (if (nil? h) newSET
                    (recur more (->> (specs-matching-less-than h nSET meta)
                                     (filter (partial none-more-general? CSET))
                                     (concat newSET)
                                     distinct))))]
          (EGS0 PSET NSET CSET NEWSET)))))

(defn EGS
  "Devuelve un concepto al azar con EGS0 de aplicado a los ejemplos
   tomando el concepto más específico como CSET y el más general como HSET"
  [ejemplos]
  (let [meta (first ejemplos)
        ejemplos+ (remove (fn [e] (= -- (last e))) (rest ejemplos))
        ejemplos- (remove (fn [e] (= ++ (last e))) (rest ejemplos))
        egs0 (EGS0 (cons meta ejemplos+)
                   (cons meta ejemplos-)
                   []
                   [(concepto-CL-mas-general meta)])]
    (prn (count egs0))
    ;;    (prn (count (nth egs0 12)))
    ;;  (prn (first (nth egs0 12)))
    egs0))
