(ns amanas17.maia.p2-5
  (:use [amanas17.maia.symbols]
        [amanas17.maia.p1-1]
        [amanas17.maia.p1-3]
        [amanas17.maia.p2-1]
        [amanas17.maia.p2-2]
        [amanas17.maia.p2-3]
        [amanas17.maia.p2-4]))

;; Ejercicio 17
(defn score-CL
  [concepto-CL PSET NSET]
  (let [Pc (->> PSET rest (map butlast) (filter (partial match-CL concepto-CL)) count)
        Nc (->> NSET rest (map butlast) (remove (partial match-CL concepto-CL)) count)]
    (/ (+ Pc Nc) (+ (dec (count PSET)) (dec (count NSET))))))


;; Ejercicio 18
(def beam-size 10)

(defn sort-by-score-desc
  [PSET NSET conceptos]
  (reverse (sort-by (fn [C] (score-CL C PSET NSET)) conceptos)))

(defn HGS0
  "Algoritmo de búsqueda heurística HGS, de general a específico,
   no incremental de conjunciones lógicas dirigido por una función de evaluación.
   Devuelve una concjunción lógica que satisface la mayoría de los ejemplos
   positivos y no satisface la mayoría de los negativos.
   Asume que los conjuntos PSET y NSET tienen cabecera de atributos"
  [PSET NSET CLOSED-SET HSET]
  (let [CLOSED-SET (atom (set CLOSED-SET))
        OPEN-SET   (atom #{})]
    (prn "First  [CLOSED-SET,HSET]=" [(count @CLOSED-SET) (count HSET)])
    (loop [[H & more] HSET]
      (when H
        (let [SPECS (mapcat (partial especializaciones-CL H (first NSET)) (rest NSET))
              NEW-SET (atom #{})]
          (loop [[S & more] SPECS]
            (when S
              (do (when (> (score-CL S PSET NSET) (score-CL H PSET NSET))
                    (swap! NEW-SET conj S))
                  (recur more))))
          (if (empty? @NEW-SET)
            (swap! CLOSED-SET conj H)
            (loop [[S & more] (seq @NEW-SET)]
              (when S
                (do (swap! OPEN-SET conj S)
                    (loop [[C & more] (seq @CLOSED-SET)]
                      (when C
                        (do (when (= -1 (cmp-concepto-CL S C)) ;; (concepto-CL>= C S)
                              (if (> (score-CL C PSET NSET) (score-CL S PSET NSET))
                                (swap! OPEN-SET   without S)
                                (swap! CLOSED-SET without C)))
                            (recur more))))
                    (recur more)))))
          (recur more))))
    (prn "Second [CLOSED-SET,OPEN-SET]=" [(count @CLOSED-SET) (count @OPEN-SET)])
    (if (empty? @OPEN-SET)
      (let [result (->> @CLOSED-SET (sort-by-score-desc PSET NSET))]
        (clojure.pprint/pprint result)
        (prn "total" (count result))
        (first result))
      (let [BEST-SET (->> @OPEN-SET (into @CLOSED-SET) (sort-by-score-desc PSET NSET)
                          (take beam-size) set)
            CLOSED-SET (filter BEST-SET @CLOSED-SET)
            OPEN-SET   (filter BEST-SET @OPEN-SET)]
        (HGS0 PSET NSET CLOSED-SET OPEN-SET)))))

(defn HGS
  "Devuelve un concepto al azar del resultado de aplicar HGS0 a los ejemplos
   tomando el concepto más específico como CSET y el más general como HSET"
  [ejemplos]
  (let [meta (first ejemplos)
        ej+ (->> ejemplos rest (filter (comp (partial = '+) last)))
        ej- (->> ejemplos rest (filter (comp (partial = '-) last)))
        hgs0 (HGS0 (cons meta ej+) (cons meta ej-) [] [(concepto-CL-mas-general meta)])]
    hgs0))

(comment (HGS ejemplos))
(comment (HGS ionosphere))
